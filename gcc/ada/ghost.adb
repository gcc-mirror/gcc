------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G H O S T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2014-2015, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Alloc;    use Alloc;
with Aspects;  use Aspects;
with Atree;    use Atree;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Table;

package body Ghost is

   --  The following table contains the N_Compilation_Unit node for a unit that
   --  is either subject to pragma Ghost with policy Ignore or contains ignored
   --  Ghost code. The table is used in the removal of ignored Ghost code from
   --  units.

   package Ignored_Ghost_Units is new Table.Table (
     Table_Component_Type => Node_Id,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Ignored_Ghost_Units_Initial,
     Table_Increment      => Alloc.Ignored_Ghost_Units_Increment,
     Table_Name           => "Ignored_Ghost_Units");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Propagate_Ignored_Ghost_Code (N : Node_Id);
   --  Subsidiary to Set_Ghost_Mode_xxx. Signal all enclosing scopes that they
   --  now contain ignored Ghost code. Add the compilation unit containing N to
   --  table Ignored_Ghost_Units for post processing.

   ----------------------------
   -- Add_Ignored_Ghost_Unit --
   ----------------------------

   procedure Add_Ignored_Ghost_Unit (Unit : Node_Id) is
   begin
      pragma Assert (Nkind (Unit) = N_Compilation_Unit);

      --  Avoid duplicates in the table as pruning the same unit more than once
      --  is wasteful. Since ignored Ghost code tends to be grouped up, check
      --  the contents of the table in reverse.

      for Index in reverse Ignored_Ghost_Units.First ..
                           Ignored_Ghost_Units.Last
      loop
         --  If the unit is already present in the table, do not add it again

         if Unit = Ignored_Ghost_Units.Table (Index) then
            return;
         end if;
      end loop;

      --  If we get here, then this is the first time the unit is being added

      Ignored_Ghost_Units.Append (Unit);
   end Add_Ignored_Ghost_Unit;

   ----------------------------
   -- Check_Ghost_Completion --
   ----------------------------

   procedure Check_Ghost_Completion
     (Partial_View : Entity_Id;
      Full_View    : Entity_Id)
   is
      Policy : constant Name_Id := Policy_In_Effect (Name_Ghost);

   begin
      --  The Ghost policy in effect at the point of declaration and at the
      --  point of completion must match (SPARK RM 6.9(15)).

      if Is_Checked_Ghost_Entity (Partial_View)
        and then Policy = Name_Ignore
      then
         Error_Msg_Sloc := Sloc (Full_View);

         Error_Msg_N ("incompatible ghost policies in effect",   Partial_View);
         Error_Msg_N ("\& declared with ghost policy Check",     Partial_View);
         Error_Msg_N ("\& completed # with ghost policy Ignore", Partial_View);

      elsif Is_Ignored_Ghost_Entity (Partial_View)
        and then Policy = Name_Check
      then
         Error_Msg_Sloc := Sloc (Full_View);

         Error_Msg_N ("incompatible ghost policies in effect",  Partial_View);
         Error_Msg_N ("\& declared with ghost policy Ignore",   Partial_View);
         Error_Msg_N ("\& completed # with ghost policy Check", Partial_View);
      end if;
   end Check_Ghost_Completion;

   -------------------------
   -- Check_Ghost_Context --
   -------------------------

   procedure Check_Ghost_Context (Ghost_Id : Entity_Id; Ghost_Ref : Node_Id) is
      procedure Check_Ghost_Policy (Id : Entity_Id; Err_N : Node_Id);
      --  Verify that the Ghost policy at the point of declaration of entity Id
      --  matches the policy at the point of reference. If this is not the case
      --  emit an error at Err_N.

      function Is_OK_Ghost_Context (Context : Node_Id) return Boolean;
      --  Determine whether node Context denotes a Ghost-friendly context where
      --  a Ghost entity can safely reside.

      -------------------------
      -- Is_OK_Ghost_Context --
      -------------------------

      function Is_OK_Ghost_Context (Context : Node_Id) return Boolean is
         function Is_Ghost_Declaration (Decl : Node_Id) return Boolean;
         --  Determine whether node Decl is a Ghost declaration or appears
         --  within a Ghost declaration.

         function Is_Ghost_Statement_Or_Pragma (N : Node_Id) return Boolean;
         --  Determine whether statement or pragma N is Ghost or appears within
         --  a Ghost statement or pragma. To qualify as such, N must either
         --    1) Occur within a ghost subprogram or package
         --    2) Denote a call to a ghost procedure
         --    3) Denote an assignment statement whose target is a ghost
         --       variable.
         --    4) Denote a pragma that mentions a ghost entity

         --------------------------
         -- Is_Ghost_Declaration --
         --------------------------

         function Is_Ghost_Declaration (Decl : Node_Id) return Boolean is
            Par       : Node_Id;
            Subp_Decl : Node_Id;
            Subp_Id   : Entity_Id;

         begin
            --  Climb the parent chain looking for an object declaration

            Par := Decl;
            while Present (Par) loop
               if Is_Declaration (Par) then

                  --  A declaration is Ghost when it appears within a Ghost
                  --  package or subprogram.

                  if Ghost_Mode > None then
                     return True;

                  --  Otherwise the declaration may not have been analyzed yet,
                  --  determine whether it is subject to aspect/pragma Ghost.

                  else
                     return Is_Subject_To_Ghost (Par);
                  end if;

               --  Special cases

               --  A reference to a Ghost entity may appear as the default
               --  expression of a formal parameter of a subprogram body. This
               --  context must be treated as suitable because the relation
               --  between the spec and the body has not been established and
               --  the body is not marked as Ghost yet. The real check was
               --  performed on the spec.

               elsif Nkind (Par) = N_Parameter_Specification
                 and then Nkind (Parent (Parent (Par))) = N_Subprogram_Body
               then
                  return True;

               --  References to Ghost entities may be relocated in internally
               --  generated bodies.

               elsif Nkind (Par) = N_Subprogram_Body
                 and then not Comes_From_Source (Par)
               then
                  Subp_Id := Corresponding_Spec (Par);

                  --  The original context is an expression function that has
                  --  been split into a spec and a body. The context is OK as
                  --  long as the the initial declaration is Ghost.

                  if Present (Subp_Id) then
                     Subp_Decl :=
                       Original_Node (Unit_Declaration_Node (Subp_Id));

                     if Nkind (Subp_Decl) = N_Expression_Function then
                        return Is_Subject_To_Ghost (Subp_Decl);
                     end if;
                  end if;

                  --  Otherwise this is either an internal body or an internal
                  --  completion. Both are OK because the real check was done
                  --  before expansion activities.

                  return True;
               end if;

               --  Prevent the search from going too far

               if Is_Body_Or_Package_Declaration (Par) then
                  return False;
               end if;

               Par := Parent (Par);
            end loop;

            return False;
         end Is_Ghost_Declaration;

         ----------------------------------
         -- Is_Ghost_Statement_Or_Pragma --
         ----------------------------------

         function Is_Ghost_Statement_Or_Pragma (N : Node_Id) return Boolean is
            function Is_Ghost_Entity_Reference (N : Node_Id) return Boolean;
            --  Determine whether an arbitrary node denotes a reference to a
            --  Ghost entity.

            -------------------------------
            -- Is_Ghost_Entity_Reference --
            -------------------------------

            function Is_Ghost_Entity_Reference (N : Node_Id) return Boolean is
               Ref : Node_Id;

            begin
               --  When the reference extracts a subcomponent, recover the
               --  related object (SPARK RM 6.9(1)).

               Ref := N;
               while Nkind_In (Ref, N_Explicit_Dereference,
                                    N_Indexed_Component,
                                    N_Selected_Component,
                                    N_Slice)
               loop
                  Ref := Prefix (Ref);
               end loop;

               return
                 Is_Entity_Name (Ref)
                   and then Present (Entity (Ref))
                   and then Is_Ghost_Entity (Entity (Ref));
            end Is_Ghost_Entity_Reference;

            --  Local variables

            Arg  : Node_Id;
            Stmt : Node_Id;

         --  Start of processing for Is_Ghost_Statement_Or_Pragma

         begin
            if Nkind (N) = N_Pragma then

               --  A pragma is Ghost when it appears within a Ghost package or
               --  subprogram.

               if Ghost_Mode > None then
                  return True;
               end if;

               --  A pragma is Ghost when it mentions a Ghost entity

               Arg := First (Pragma_Argument_Associations (N));
               while Present (Arg) loop
                  if Is_Ghost_Entity_Reference (Get_Pragma_Arg (Arg)) then
                     return True;
                  end if;

                  Next (Arg);
               end loop;
            end if;

            Stmt := N;
            while Present (Stmt) loop
               if Is_Statement (Stmt) then

                  --  A statement is Ghost when it appears within a Ghost
                  --  package or subprogram.

                  if Ghost_Mode > None then
                     return True;

                  --  An assignment statement or a procedure call is Ghost when
                  --  the name denotes a Ghost entity.

                  elsif Nkind_In (Stmt, N_Assignment_Statement,
                                        N_Procedure_Call_Statement)
                  then
                     return Is_Ghost_Entity_Reference (Name (Stmt));
                  end if;

               --  Prevent the search from going too far

               elsif Is_Body_Or_Package_Declaration (Stmt) then
                  return False;
               end if;

               Stmt := Parent (Stmt);
            end loop;

            return False;
         end Is_Ghost_Statement_Or_Pragma;

      --  Start of processing for Is_OK_Ghost_Context

      begin
         --  The Ghost entity appears within an assertion expression

         if In_Assertion_Expr > 0 then
            return True;

         --  The Ghost entity is part of a declaration or its completion

         elsif Is_Ghost_Declaration (Context) then
            return True;

         --  The Ghost entity is referenced within a Ghost statement

         elsif Is_Ghost_Statement_Or_Pragma (Context) then
            return True;

         --  Routine Expand_Record_Extension creates a parent subtype without
         --  inserting it into the tree. There is no good way of recognizing
         --  this special case as there is no parent. Try to approximate the
         --  context.

         elsif No (Parent (Context)) and then Is_Tagged_Type (Ghost_Id) then
            return True;

         else
            return False;
         end if;
      end Is_OK_Ghost_Context;

      ------------------------
      -- Check_Ghost_Policy --
      ------------------------

      procedure Check_Ghost_Policy (Id : Entity_Id; Err_N : Node_Id) is
         Policy : constant Name_Id := Policy_In_Effect (Name_Ghost);

      begin
         --  The Ghost policy in effect a the point of declaration and at the
         --  point of use must match (SPARK RM 6.9(14)).

         if Is_Checked_Ghost_Entity (Id) and then Policy = Name_Ignore then
            Error_Msg_Sloc := Sloc (Err_N);

            Error_Msg_N  ("incompatible ghost policies in effect", Err_N);
            Error_Msg_NE ("\& declared with ghost policy Check", Err_N, Id);
            Error_Msg_NE ("\& used # with ghost policy Ignore", Err_N, Id);

         elsif Is_Ignored_Ghost_Entity (Id) and then Policy = Name_Check then
            Error_Msg_Sloc := Sloc (Err_N);

            Error_Msg_N  ("incompatible ghost policies in effect", Err_N);
            Error_Msg_NE ("\& declared with ghost policy Ignore", Err_N, Id);
            Error_Msg_NE ("\& used # with ghost policy Check", Err_N, Id);
         end if;
      end Check_Ghost_Policy;

   --  Start of processing for Check_Ghost_Context

   begin
      --  Once it has been established that the reference to the Ghost entity
      --  is within a suitable context, ensure that the policy at the point of
      --  declaration and at the point of use match.

      if Is_OK_Ghost_Context (Ghost_Ref) then
         Check_Ghost_Policy (Ghost_Id, Ghost_Ref);

      --  Otherwise the Ghost entity appears in a non-Ghost context and affects
      --  its behavior or value.

      else
         Error_Msg_N
           ("ghost entity cannot appear in this context (SPARK RM 6.9(12))",
            Ghost_Ref);
      end if;
   end Check_Ghost_Context;

   ----------------------------
   -- Check_Ghost_Derivation --
   ----------------------------

   procedure Check_Ghost_Derivation (Typ : Entity_Id) is
      Parent_Typ : constant Entity_Id := Etype (Typ);
      Iface      : Entity_Id;
      Iface_Elmt : Elmt_Id;

   begin
      --  Allow untagged derivations from predefined types such as Integer as
      --  those are not Ghost by definition.

      if Is_Scalar_Type (Typ) and then Parent_Typ = Base_Type (Typ) then
         null;

      --  The parent type of a Ghost type extension must be Ghost

      elsif not Is_Ghost_Entity (Parent_Typ) then
         Error_Msg_N  ("type extension & cannot be ghost", Typ);
         Error_Msg_NE ("\parent type & is not ghost", Typ, Parent_Typ);
         return;
      end if;

      --  All progenitors (if any) must be Ghost as well

      if Is_Tagged_Type (Typ) and then Present (Interfaces (Typ)) then
         Iface_Elmt := First_Elmt (Interfaces (Typ));
         while Present (Iface_Elmt) loop
            Iface := Node (Iface_Elmt);

            if not Is_Ghost_Entity (Iface) then
               Error_Msg_N  ("type extension & cannot be ghost", Typ);
               Error_Msg_NE ("\interface type & is not ghost", Typ, Iface);
               return;
            end if;

            Next_Elmt (Iface_Elmt);
         end loop;
      end if;
   end Check_Ghost_Derivation;

   --------------------------------
   -- Implements_Ghost_Interface --
   --------------------------------

   function Implements_Ghost_Interface (Typ : Entity_Id) return Boolean is
      Iface_Elmt : Elmt_Id;

   begin
      --  Traverse the list of interfaces looking for a Ghost interface

      if Is_Tagged_Type (Typ) and then Present (Interfaces (Typ)) then
         Iface_Elmt := First_Elmt (Interfaces (Typ));
         while Present (Iface_Elmt) loop
            if Is_Ghost_Entity (Node (Iface_Elmt)) then
               return True;
            end if;

            Next_Elmt (Iface_Elmt);
         end loop;
      end if;

      return False;
   end Implements_Ghost_Interface;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Ignored_Ghost_Units.Init;
   end Initialize;

   ---------------------
   -- Is_Ghost_Entity --
   ---------------------

   function Is_Ghost_Entity (Id : Entity_Id) return Boolean is
   begin
      return Is_Checked_Ghost_Entity (Id) or else Is_Ignored_Ghost_Entity (Id);
   end Is_Ghost_Entity;

   -------------------------
   -- Is_Subject_To_Ghost --
   -------------------------

   function Is_Subject_To_Ghost (N : Node_Id) return Boolean is
      function Enables_Ghostness (Arg : Node_Id) return Boolean;
      --  Determine whether aspect or pragma argument Arg enables "ghostness"

      -----------------------
      -- Enables_Ghostness --
      -----------------------

      function Enables_Ghostness (Arg : Node_Id) return Boolean is
         Expr : Node_Id;

      begin
         Expr := Arg;

         if Nkind (Expr) = N_Pragma_Argument_Association then
            Expr := Get_Pragma_Arg (Expr);
         end if;

         --  Determine whether the expression of the aspect is static and
         --  denotes True.

         if Present (Expr) then
            Preanalyze_And_Resolve (Expr);

            return
              Is_OK_Static_Expression (Expr)
                and then Is_True (Expr_Value (Expr));

         --  Otherwise Ghost defaults to True

         else
            return True;
         end if;
      end Enables_Ghostness;

      --  Local variables

      Id      : constant Entity_Id := Defining_Entity (N);
      Asp     : Node_Id;
      Decl    : Node_Id;
      Prev_Id : Entity_Id;

   --  Start of processing for Is_Subject_To_Ghost

   begin
      --  The related entity of the declaration has not been analyzed yet, do
      --  not inspect its attributes.

      if Ekind (Id) = E_Void then
         null;

      elsif Is_Ghost_Entity (Id) then
         return True;

      --  The completion of a type or a constant is not fully analyzed when the
      --  reference to the Ghost entity is resolved. Because the completion is
      --  not marked as Ghost yet, inspect the partial view.

      elsif Is_Record_Type (Id)
        or else Ekind (Id) = E_Constant
        or else (Nkind (N) = N_Object_Declaration
                  and then Constant_Present (N))
      then
         Prev_Id := Incomplete_Or_Partial_View (Id);

         if Present (Prev_Id) and then Is_Ghost_Entity (Prev_Id) then
            return True;
         end if;
      end if;

      --  Examine the aspect specifications (if any) looking for aspect Ghost

      if Permits_Aspect_Specifications (N) then
         Asp := First (Aspect_Specifications (N));
         while Present (Asp) loop
            if Chars (Identifier (Asp)) = Name_Ghost then
               return Enables_Ghostness (Expression (Asp));
            end if;

            Next (Asp);
         end loop;
      end if;

      Decl := Empty;

      --  When the context is a [generic] package declaration, pragma Ghost
      --  resides in the visible declarations.

      if Nkind_In (N, N_Generic_Package_Declaration,
                      N_Package_Declaration)
      then
         Decl := First (Visible_Declarations (Specification (N)));

      --  When the context is a package or a subprogram body, pragma Ghost
      --  resides in the declarative part.

      elsif Nkind_In (N, N_Package_Body, N_Subprogram_Body) then
         Decl := First (Declarations (N));

      --  Otherwise pragma Ghost appears in the declarations following N

      elsif Is_List_Member (N) then
         Decl := Next (N);
      end if;

      while Present (Decl) loop
         if Nkind (Decl) = N_Pragma
           and then Pragma_Name (Decl) = Name_Ghost
         then
            return
              Enables_Ghostness (First (Pragma_Argument_Associations (Decl)));

         --  A source construct ends the region where pragma Ghost may appear,
         --  stop the traversal.

         elsif Comes_From_Source (Decl) then
            exit;
         end if;

         Next (Decl);
      end loop;

      return False;
   end Is_Subject_To_Ghost;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Ignored_Ghost_Units.Locked := True;
      Ignored_Ghost_Units.Release;
   end Lock;

   ----------------------------------
   -- Propagate_Ignored_Ghost_Code --
   ----------------------------------

   procedure Propagate_Ignored_Ghost_Code (N : Node_Id) is
      Nod  : Node_Id;
      Scop : Entity_Id;

   begin
      --  Traverse the parent chain looking for blocks, packages and
      --  subprograms or their respective bodies.

      Nod := Parent (N);
      while Present (Nod) loop
         Scop := Empty;

         if Nkind (Nod) = N_Block_Statement then
            Scop := Entity (Identifier (Nod));

         elsif Nkind_In (Nod, N_Package_Body,
                              N_Package_Declaration,
                              N_Subprogram_Body,
                              N_Subprogram_Declaration)
         then
            Scop := Defining_Entity (Nod);
         end if;

         --  The current node denotes a scoping construct

         if Present (Scop) then

            --  Stop the traversal when the scope already contains ignored
            --  Ghost code as all enclosing scopes have already been marked.

            if Contains_Ignored_Ghost_Code (Scop) then
               exit;

            --  Otherwise mark this scope and keep climbing

            else
               Set_Contains_Ignored_Ghost_Code (Scop);
            end if;
         end if;

         Nod := Parent (Nod);
      end loop;

      --  The unit containing the ignored Ghost code must be post processed
      --  before invoking the back end.

      Add_Ignored_Ghost_Unit (Cunit (Get_Code_Unit (N)));
   end Propagate_Ignored_Ghost_Code;

   -------------------------------
   -- Remove_Ignored_Ghost_Code --
   -------------------------------

   procedure Remove_Ignored_Ghost_Code is
      procedure Prune_Tree (Root : Node_Id);
      --  Remove all code marked as ignored Ghost from the tree of denoted by
      --  Root.

      ----------------
      -- Prune_Tree --
      ----------------

      procedure Prune_Tree (Root : Node_Id) is
         procedure Prune (N : Node_Id);
         --  Remove a given node from the tree by rewriting it into null

         function Prune_Node (N : Node_Id) return Traverse_Result;
         --  Determine whether node N denotes an ignored Ghost construct. If
         --  this is the case, rewrite N as a null statement. See the body for
         --  special cases.

         -----------
         -- Prune --
         -----------

         procedure Prune (N : Node_Id) is
         begin
            --  Destroy any aspects that may be associated with the node

            if Permits_Aspect_Specifications (N) and then Has_Aspects (N) then
               Remove_Aspects (N);
            end if;

            Rewrite (N, Make_Null_Statement (Sloc (N)));
         end Prune;

         ----------------
         -- Prune_Node --
         ----------------

         function Prune_Node (N : Node_Id) return Traverse_Result is
            Id : Entity_Id;

         begin
            --  The node is either declared as ignored Ghost or is a byproduct
            --  of expansion. Destroy it and stop the traversal on this branch.

            if Is_Ignored_Ghost_Node (N) then
               Prune (N);
               return Skip;

            --  Scoping constructs such as blocks, packages, subprograms and
            --  bodies offer some flexibility with respect to pruning.

            elsif Nkind_In (N, N_Block_Statement,
                               N_Package_Body,
                               N_Package_Declaration,
                               N_Subprogram_Body,
                               N_Subprogram_Declaration)
            then
               if Nkind (N) = N_Block_Statement then
                  Id := Entity (Identifier (N));
               else
                  Id := Defining_Entity (N);
               end if;

               --  The scoping construct contains both living and ignored Ghost
               --  code, let the traversal prune all relevant nodes.

               if Contains_Ignored_Ghost_Code (Id) then
                  return OK;

               --  Otherwise the construct contains only living code and should
               --  not be pruned.

               else
                  return Skip;
               end if;

            --  Otherwise keep searching for ignored Ghost nodes

            else
               return OK;
            end if;
         end Prune_Node;

         procedure Prune_Nodes is new Traverse_Proc (Prune_Node);

      --  Start of processing for Prune_Tree

      begin
         Prune_Nodes (Root);
      end Prune_Tree;

   --  Start of processing for Remove_Ignored_Ghost_Code

   begin
      for Index in Ignored_Ghost_Units.First .. Ignored_Ghost_Units.Last loop
         Prune_Tree (Unit (Ignored_Ghost_Units.Table (Index)));
      end loop;
   end Remove_Ignored_Ghost_Code;

   --------------------
   -- Set_Ghost_Mode --
   --------------------

   procedure Set_Ghost_Mode (N : Node_Id; Prev_Id : Entity_Id := Empty) is
      procedure Set_Ghost_Mode_From_Entity (Id : Entity_Id);
      --  Set the value of global variable Ghost_Mode depending on the mode of
      --  entity Id.

      procedure Set_Ghost_Mode_From_Policy;
      --  Set the value of global variable Ghost_Mode depending on the current
      --  Ghost policy in effect.

      --------------------------------
      -- Set_Ghost_Mode_From_Entity --
      --------------------------------

      procedure Set_Ghost_Mode_From_Entity (Id : Entity_Id) is
      begin
         if Is_Checked_Ghost_Entity (Id) then
            Ghost_Mode := Check;

         elsif Is_Ignored_Ghost_Entity (Id) then
            Ghost_Mode := Ignore;

            Set_Is_Ignored_Ghost_Node (N);
            Propagate_Ignored_Ghost_Code (N);
         end if;
      end Set_Ghost_Mode_From_Entity;

      --------------------------------
      -- Set_Ghost_Mode_From_Policy --
      --------------------------------

      procedure Set_Ghost_Mode_From_Policy is
         Policy : constant Name_Id := Policy_In_Effect (Name_Ghost);

      begin
         if Policy = Name_Check then
            Ghost_Mode := Check;

         elsif Policy = Name_Ignore then
            Ghost_Mode := Ignore;

            Set_Is_Ignored_Ghost_Node (N);
            Propagate_Ignored_Ghost_Code (N);
         end if;
      end Set_Ghost_Mode_From_Policy;

      --  Local variables

      Nam : Node_Id;

   --  Start of processing for Set_Ghost_Mode

   begin
      --  The input node denotes one of the many declaration kinds that may be
      --  subject to pragma Ghost.

      if Is_Declaration (N) then
         if Is_Subject_To_Ghost (N) then
            Set_Ghost_Mode_From_Policy;

         --  The declaration denotes the completion of a deferred constant,
         --  pragma Ghost appears on the partial declaration.

         elsif Nkind (N) = N_Object_Declaration
           and then Constant_Present (N)
           and then Present (Prev_Id)
         then
            Set_Ghost_Mode_From_Entity (Prev_Id);

         --  The declaration denotes the full view of a private type, pragma
         --  Ghost appears on the partial declaration.

         elsif Nkind (N) = N_Full_Type_Declaration
           and then Is_Private_Type (Defining_Entity (N))
           and then Present (Prev_Id)
         then
            Set_Ghost_Mode_From_Entity (Prev_Id);
         end if;

      --  The input denotes an assignment or a procedure call. In this case
      --  the Ghost mode is dictated by the name of the construct.

      elsif Nkind_In (N, N_Assignment_Statement,
                         N_Procedure_Call_Statement)
      then
         --  When the reference extracts a subcomponent, recover the related
         --  object (SPARK RM 6.9(1)).

         Nam := Name (N);
         while Nkind_In (Nam, N_Explicit_Dereference,
                              N_Indexed_Component,
                              N_Selected_Component,
                              N_Slice)
         loop
            Nam := Prefix (Nam);
         end loop;

         if Is_Entity_Name (Nam)
           and then Present (Entity (Nam))
         then
            Set_Ghost_Mode_From_Entity (Entity (Nam));
         end if;

      --  The input denotes a package or subprogram body

      elsif Nkind_In (N, N_Package_Body, N_Subprogram_Body) then
         if (Present (Prev_Id) and then Is_Ghost_Entity (Prev_Id))
           or else Is_Subject_To_Ghost (N)
         then
            Set_Ghost_Mode_From_Policy;
         end if;
      end if;
   end Set_Ghost_Mode;

   -------------------------------
   -- Set_Ghost_Mode_For_Freeze --
   -------------------------------

   procedure Set_Ghost_Mode_For_Freeze (Id : Entity_Id; N : Node_Id) is
   begin
      if Is_Checked_Ghost_Entity (Id) then
         Ghost_Mode := Check;
      elsif Is_Ignored_Ghost_Entity (Id) then
         Ghost_Mode := Ignore;
         Propagate_Ignored_Ghost_Code (N);
      end if;
   end Set_Ghost_Mode_For_Freeze;

   -------------------------
   -- Set_Is_Ghost_Entity --
   -------------------------

   procedure Set_Is_Ghost_Entity (Id : Entity_Id) is
      Policy : constant Name_Id := Policy_In_Effect (Name_Ghost);
   begin
      if Policy = Name_Check then
         Set_Is_Checked_Ghost_Entity (Id);
      elsif Policy = Name_Ignore then
         Set_Is_Ignored_Ghost_Entity (Id);
      end if;
   end Set_Is_Ghost_Entity;

end Ghost;
