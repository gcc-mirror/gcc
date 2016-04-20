------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G H O S T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2014-2015, Free Software Foundation, Inc.         --
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
with Sem_Disp; use Sem_Disp;
with Sem_Eval; use Sem_Eval;
with Sem_Prag; use Sem_Prag;
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

   function Ghost_Entity (N : Node_Id) return Entity_Id;
   --  Subsidiary to Check_Ghost_Context and Set_Ghost_Mode. Find the entity of
   --  a reference to a Ghost entity. Return Empty if there is no such entity.

   function Is_Subject_To_Ghost (N : Node_Id) return Boolean;
   --  Subsidiary to routines Is_OK_xxx and Set_Ghost_Mode. Determine whether
   --  declaration or body N is subject to aspect or pragma Ghost. Use this
   --  routine in cases where [source] pragma Ghost has not been analyzed yet,
   --  but the context needs to establish the "ghostness" of N.

   procedure Propagate_Ignored_Ghost_Code (N : Node_Id);
   --  Subsidiary to routines Mark_xxx_As_Ghost and Set_Ghost_Mode_From_xxx.
   --  Signal all enclosing scopes that they now contain ignored Ghost code.
   --  Add the compilation unit containing N to table Ignored_Ghost_Units for
   --  post processing.

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
      --  point of completion must match (SPARK RM 6.9(14)).

      if Is_Checked_Ghost_Entity (Partial_View)
        and then Policy = Name_Ignore
      then
         Error_Msg_Sloc := Sloc (Full_View);

         Error_Msg_N ("incompatible ghost policies in effect", Partial_View);
         Error_Msg_N ("\& declared with ghost policy `Check`", Partial_View);
         Error_Msg_N ("\& completed # with ghost policy `Ignore`",
                                                               Partial_View);

      elsif Is_Ignored_Ghost_Entity (Partial_View)
        and then Policy = Name_Check
      then
         Error_Msg_Sloc := Sloc (Full_View);

         Error_Msg_N ("incompatible ghost policies in effect",  Partial_View);
         Error_Msg_N ("\& declared with ghost policy `Ignore`", Partial_View);
         Error_Msg_N ("\& completed # with ghost policy `Check`",
                                                                Partial_View);
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
      --  a Ghost entity can safely reside (SPARK RM 6.9(10)).

      -------------------------
      -- Is_OK_Ghost_Context --
      -------------------------

      function Is_OK_Ghost_Context (Context : Node_Id) return Boolean is
         function Is_OK_Declaration (Decl : Node_Id) return Boolean;
         --  Determine whether node Decl is a suitable context for a reference
         --  to a Ghost entity. To qualify as such, Decl must either
         --    1) Be subject to pragma Ghost
         --    2) Rename a Ghost entity

         function Is_OK_Pragma (Prag : Node_Id) return Boolean;
         --  Determine whether node Prag is a suitable context for a reference
         --  to a Ghost entity. To qualify as such, Prag must either
         --    1) Be an assertion expression pragma
         --    2) Denote pragma Global, Depends, Initializes, Refined_Global,
         --       Refined_Depends or Refined_State
         --    3) Specify an aspect of a Ghost entity
         --    4) Contain a reference to a Ghost entity

         function Is_OK_Statement (Stmt : Node_Id) return Boolean;
         --  Determine whether node Stmt is a suitable context for a reference
         --  to a Ghost entity. To qualify as such, Stmt must either
         --    1) Denote a call to a Ghost procedure
         --    2) Denote an assignment statement whose target is Ghost

         -----------------------
         -- Is_OK_Declaration --
         -----------------------

         function Is_OK_Declaration (Decl : Node_Id) return Boolean is
            function Is_Ghost_Renaming (Ren_Decl : Node_Id) return Boolean;
            --  Determine whether node Ren_Decl denotes a renaming declaration
            --  with a Ghost name.

            -----------------------
            -- Is_Ghost_Renaming --
            -----------------------

            function Is_Ghost_Renaming (Ren_Decl : Node_Id) return Boolean is
               Nam_Id : Entity_Id;

            begin
               if Is_Renaming_Declaration (Ren_Decl) then
                  Nam_Id := Ghost_Entity (Name (Ren_Decl));

                  return Present (Nam_Id) and then Is_Ghost_Entity (Nam_Id);
               end if;

               return False;
            end Is_Ghost_Renaming;

            --  Local variables

            Subp_Decl : Node_Id;
            Subp_Id   : Entity_Id;

         --  Start of processing for Is_OK_Declaration

         begin
            if Is_Declaration (Decl) then

               --  A renaming declaration is Ghost when it renames a Ghost
               --  entity.

               if Is_Ghost_Renaming (Decl) then
                  return True;

               --  The declaration may not have been analyzed yet, determine
               --  whether it is subject to pragma Ghost.

               elsif Is_Subject_To_Ghost (Decl) then
                  return True;
               end if;

            --  Special cases

            --  A reference to a Ghost entity may appear as the default
            --  expression of a formal parameter of a subprogram body. This
            --  context must be treated as suitable because the relation
            --  between the spec and the body has not been established and
            --  the body is not marked as Ghost yet. The real check was
            --  performed on the spec.

            elsif Nkind (Decl) = N_Parameter_Specification
              and then Nkind (Parent (Parent (Decl))) = N_Subprogram_Body
            then
               return True;

            --  References to Ghost entities may be relocated in internally
            --  generated bodies.

            elsif Nkind (Decl) = N_Subprogram_Body
              and then not Comes_From_Source (Decl)
            then
               Subp_Id := Corresponding_Spec (Decl);

               if Present (Subp_Id) then

                  --  The context is the internally built _postconditions
                  --  subprogram, which it is OK because the real check was
                  --  done before expansion activities.

                  if Chars (Subp_Id) = Name_uPostconditions then
                     return True;

                  else
                     Subp_Decl :=
                       Original_Node (Unit_Declaration_Node (Subp_Id));

                     --  The original context is an expression function that
                     --  has been split into a spec and a body. The context is
                     --  OK as long as the initial declaration is Ghost.

                     if Nkind (Subp_Decl) = N_Expression_Function then
                        return Is_Subject_To_Ghost (Subp_Decl);
                     end if;
                  end if;

               --  Otherwise this is either an internal body or an internal
               --  completion. Both are OK because the real check was done
               --  before expansion activities.

               else
                  return True;
               end if;
            end if;

            return False;
         end Is_OK_Declaration;

         ------------------
         -- Is_OK_Pragma --
         ------------------

         function Is_OK_Pragma (Prag : Node_Id) return Boolean is
            procedure Check_Policies (Prag_Nam : Name_Id);
            --  Verify that the Ghost policy in effect is the same as the
            --  assertion policy for pragma name Prag_Nam. Emit an error if
            --  this is not the case.

            --------------------
            -- Check_Policies --
            --------------------

            procedure Check_Policies (Prag_Nam : Name_Id) is
               AP : constant Name_Id := Check_Kind (Prag_Nam);
               GP : constant Name_Id := Policy_In_Effect (Name_Ghost);

            begin
               --  If the Ghost policy in effect at the point of a Ghost entity
               --  reference is Ignore, then the assertion policy of the pragma
               --  must be Ignore (SPARK RM 6.9(18)).

               if GP = Name_Ignore and then AP /= Name_Ignore then
                  Error_Msg_N
                    ("incompatible ghost policies in effect",
                     Ghost_Ref);
                  Error_Msg_NE
                    ("\ghost entity & has policy `Ignore`",
                     Ghost_Ref, Ghost_Id);

                  Error_Msg_Name_1 := AP;
                  Error_Msg_N
                    ("\assertion expression has policy %", Ghost_Ref);
               end if;
            end Check_Policies;

            --  Local variables

            Arg      : Node_Id;
            Arg_Id   : Entity_Id;
            Prag_Id  : Pragma_Id;
            Prag_Nam : Name_Id;

         --  Start of processing for Is_OK_Pragma

         begin
            if Nkind (Prag) = N_Pragma then
               Prag_Id  := Get_Pragma_Id (Prag);
               Prag_Nam := Original_Aspect_Pragma_Name (Prag);

               --  A pragma that applies to a Ghost construct or specifies an
               --  aspect of a Ghost entity is a Ghost pragma (SPARK RM 6.9(3))

               if Is_Ghost_Pragma (Prag) then
                  return True;

               --  An assertion expression pragma is Ghost when it contains a
               --  reference to a Ghost entity (SPARK RM 6.9(10)).

               elsif Assertion_Expression_Pragma (Prag_Id) then

                  --  Ensure that the assertion policy and the Ghost policy are
                  --  compatible (SPARK RM 6.9(18)).

                  Check_Policies (Prag_Nam);
                  return True;

               --  Several pragmas that may apply to a non-Ghost entity are
               --  treated as Ghost when they contain a reference to a Ghost
               --  entity (SPARK RM 6.9(11)).

               elsif Nam_In (Prag_Nam, Name_Global,
                                       Name_Depends,
                                       Name_Initializes,
                                       Name_Refined_Global,
                                       Name_Refined_Depends,
                                       Name_Refined_State)
               then
                  return True;

               --  Otherwise a normal pragma is Ghost when it encloses a Ghost
               --  name (SPARK RM 6.9(3)).

               else
                  Arg := First (Pragma_Argument_Associations (Prag));
                  while Present (Arg) loop
                     Arg_Id := Ghost_Entity (Get_Pragma_Arg (Arg));

                     if Present (Arg_Id) and then Is_Ghost_Entity (Arg_Id) then
                        return True;
                     end if;

                     Next (Arg);
                  end loop;
               end if;
            end if;

            return False;
         end Is_OK_Pragma;

         ---------------------
         -- Is_OK_Statement --
         ---------------------

         function Is_OK_Statement (Stmt : Node_Id) return Boolean is
            Nam_Id : Entity_Id;

         begin
            --  An assignment statement or a procedure call is Ghost when the
            --  name denotes a Ghost entity.

            if Nkind_In (Stmt, N_Assignment_Statement,
                               N_Procedure_Call_Statement)
            then
               Nam_Id := Ghost_Entity (Name (Stmt));

               return Present (Nam_Id) and then Is_Ghost_Entity (Nam_Id);

            --  Special cases

            --  An if statement is a suitable context for a Ghost entity if it
            --  is the byproduct of assertion expression expansion. Note that
            --  the assertion expression may not be related to a Ghost entity,
            --  but it may still contain references to Ghost entities.

            elsif Nkind (Stmt) = N_If_Statement
              and then Nkind (Original_Node (Stmt)) = N_Pragma
              and then Assertion_Expression_Pragma
                         (Get_Pragma_Id (Original_Node (Stmt)))
            then
               return True;
            end if;

            return False;
         end Is_OK_Statement;

         --  Local variables

         Par : Node_Id;

      --  Start of processing for Is_OK_Ghost_Context

      begin
         --  The context is Ghost when it appears within a Ghost package or
         --  subprogram.

         if Ghost_Mode > None then
            return True;

         --  Routine Expand_Record_Extension creates a parent subtype without
         --  inserting it into the tree. There is no good way of recognizing
         --  this special case as there is no parent. Try to approximate the
         --  context.

         elsif No (Parent (Context)) and then Is_Tagged_Type (Ghost_Id) then
            return True;

         --  Otherwise climb the parent chain looking for a suitable Ghost
         --  context.

         else
            Par := Context;
            while Present (Par) loop
               if Is_Ignored_Ghost_Node (Par) then
                  return True;

               --  A reference to a Ghost entity can appear within an aspect
               --  specification (SPARK RM 6.9(10)).

               elsif Nkind (Par) = N_Aspect_Specification then
                  return True;

               elsif Is_OK_Declaration (Par) then
                  return True;

               elsif Is_OK_Pragma (Par) then
                  return True;

               elsif Is_OK_Statement (Par) then
                  return True;

               --  Prevent the search from going too far

               elsif Is_Body_Or_Package_Declaration (Par) then
                  exit;
               end if;

               Par := Parent (Par);
            end loop;

            --  The expansion of assertion expression pragmas and attribute Old
            --  may cause a legal Ghost entity reference to become illegal due
            --  to node relocation. Check the In_Assertion_Expr counter as last
            --  resort to try and infer the original legal context.

            if In_Assertion_Expr > 0 then
               return True;

            --  Otherwise the context is not suitable for a reference to a
            --  Ghost entity.

            else
               return False;
            end if;
         end if;
      end Is_OK_Ghost_Context;

      ------------------------
      -- Check_Ghost_Policy --
      ------------------------

      procedure Check_Ghost_Policy (Id : Entity_Id; Err_N : Node_Id) is
         Policy : constant Name_Id := Policy_In_Effect (Name_Ghost);

      begin
         --  The Ghost policy in effect a the point of declaration and at the
         --  point of use must match (SPARK RM 6.9(13)).

         if Is_Checked_Ghost_Entity (Id) and then Policy = Name_Ignore then
            Error_Msg_Sloc := Sloc (Err_N);

            Error_Msg_N  ("incompatible ghost policies in effect", Err_N);
            Error_Msg_NE ("\& declared with ghost policy `Check`", Err_N, Id);
            Error_Msg_NE ("\& used # with ghost policy `Ignore`",  Err_N, Id);

         elsif Is_Ignored_Ghost_Entity (Id) and then Policy = Name_Check then
            Error_Msg_Sloc := Sloc (Err_N);

            Error_Msg_N  ("incompatible ghost policies in effect",  Err_N);
            Error_Msg_NE ("\& declared with ghost policy `Ignore`", Err_N, Id);
            Error_Msg_NE ("\& used # with ghost policy `Check`",    Err_N, Id);
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
      --  its behavior or value (SPARK RM 6.9(11,12)).

      else
         Error_Msg_N ("ghost entity cannot appear in this context", Ghost_Ref);
      end if;
   end Check_Ghost_Context;

   ----------------------------
   -- Check_Ghost_Overriding --
   ----------------------------

   procedure Check_Ghost_Overriding
     (Subp            : Entity_Id;
      Overridden_Subp : Entity_Id)
   is
      Deriv_Typ : Entity_Id;
      Over_Subp : Entity_Id;

   begin
      if Present (Subp) and then Present (Overridden_Subp) then
         Over_Subp := Ultimate_Alias (Overridden_Subp);
         Deriv_Typ := Find_Dispatching_Type (Subp);

         --  A Ghost primitive of a non-Ghost type extension cannot override an
         --  inherited non-Ghost primitive (SPARK RM 6.9(8)).

         if Is_Ghost_Entity (Subp)
           and then Present (Deriv_Typ)
           and then not Is_Ghost_Entity (Deriv_Typ)
           and then not Is_Ghost_Entity (Over_Subp)
         then
            Error_Msg_N ("incompatible overriding in effect", Subp);

            Error_Msg_Sloc := Sloc (Over_Subp);
            Error_Msg_N ("\& declared # as non-ghost subprogram", Subp);

            Error_Msg_Sloc := Sloc (Subp);
            Error_Msg_N ("\overridden # with ghost subprogram", Subp);
         end if;

         --  A non-Ghost primitive of a type extension cannot override an
         --  inherited Ghost primitive (SPARK RM 6.9(8)).

         if not Is_Ghost_Entity (Subp)
           and then Is_Ghost_Entity (Over_Subp)
         then
            Error_Msg_N ("incompatible overriding in effect", Subp);

            Error_Msg_Sloc := Sloc (Over_Subp);
            Error_Msg_N ("\& declared # as ghost subprogram", Subp);

            Error_Msg_Sloc := Sloc (Subp);
            Error_Msg_N ("\overridden # with non-ghost subprogram", Subp);
         end if;

         if Present (Deriv_Typ)
           and then not Is_Ignored_Ghost_Entity (Deriv_Typ)
         then
            --  When a tagged type is either non-Ghost or checked Ghost and
            --  one of its primitives overrides an inherited operation, the
            --  overridden operation of the ancestor type must be ignored Ghost
            --  if the primitive is ignored Ghost (SPARK RM 6.9(17)).

            if Is_Ignored_Ghost_Entity (Subp) then

               --  Both the parent subprogram and overriding subprogram are
               --  ignored Ghost.

               if Is_Ignored_Ghost_Entity (Over_Subp) then
                  null;

               --  The parent subprogram carries policy Check

               elsif Is_Checked_Ghost_Entity (Over_Subp) then
                  Error_Msg_N
                    ("incompatible ghost policies in effect", Subp);

                  Error_Msg_Sloc := Sloc (Over_Subp);
                  Error_Msg_N
                    ("\& declared # with ghost policy `Check`", Subp);

                  Error_Msg_Sloc := Sloc (Subp);
                  Error_Msg_N
                    ("\overridden # with ghost policy `Ignore`", Subp);

               --  The parent subprogram is non-Ghost

               else
                  Error_Msg_N
                    ("incompatible ghost policies in effect", Subp);

                  Error_Msg_Sloc := Sloc (Over_Subp);
                  Error_Msg_N ("\& declared # as non-ghost subprogram", Subp);

                  Error_Msg_Sloc := Sloc (Subp);
                  Error_Msg_N
                    ("\overridden # with ghost policy `Ignore`", Subp);
               end if;

            --  When a tagged type is either non-Ghost or checked Ghost and
            --  one of its primitives overrides an inherited operation, the
            --  the primitive of the tagged type must be ignored Ghost if the
            --  overridden operation is ignored Ghost (SPARK RM 6.9(17)).

            elsif Is_Ignored_Ghost_Entity (Over_Subp) then

               --  Both the parent subprogram and the overriding subprogram are
               --  ignored Ghost.

               if Is_Ignored_Ghost_Entity (Subp) then
                  null;

               --  The overriding subprogram carries policy Check

               elsif Is_Checked_Ghost_Entity (Subp) then
                  Error_Msg_N
                    ("incompatible ghost policies in effect", Subp);

                  Error_Msg_Sloc := Sloc (Over_Subp);
                  Error_Msg_N
                    ("\& declared # with ghost policy `Ignore`", Subp);

                  Error_Msg_Sloc := Sloc (Subp);
                  Error_Msg_N
                    ("\overridden # with Ghost policy `Check`", Subp);

               --  The overriding subprogram is non-Ghost

               else
                  Error_Msg_N
                    ("incompatible ghost policies in effect", Subp);

                  Error_Msg_Sloc := Sloc (Over_Subp);
                  Error_Msg_N
                    ("\& declared # with ghost policy `Ignore`", Subp);

                  Error_Msg_Sloc := Sloc (Subp);
                  Error_Msg_N
                    ("\overridden # with non-ghost subprogram", Subp);
               end if;
            end if;
         end if;
      end if;
   end Check_Ghost_Overriding;

   ---------------------------
   -- Check_Ghost_Primitive --
   ---------------------------

   procedure Check_Ghost_Primitive (Prim : Entity_Id; Typ : Entity_Id) is
   begin
      --  The Ghost policy in effect at the point of declaration of a primitive
      --  operation and a tagged type must match (SPARK RM 6.9(16)).

      if Is_Tagged_Type (Typ) then
         if Is_Checked_Ghost_Entity (Prim)
           and then Is_Ignored_Ghost_Entity (Typ)
         then
            Error_Msg_N ("incompatible ghost policies in effect", Prim);

            Error_Msg_Sloc := Sloc (Typ);
            Error_Msg_NE
              ("\tagged type & declared # with ghost policy `Ignore`",
               Prim, Typ);

            Error_Msg_Sloc := Sloc (Prim);
            Error_Msg_N
              ("\primitive subprogram & declared # with ghost policy `Check`",
               Prim);

         elsif Is_Ignored_Ghost_Entity (Prim)
           and then Is_Checked_Ghost_Entity (Typ)
         then
            Error_Msg_N ("incompatible ghost policies in effect", Prim);

            Error_Msg_Sloc := Sloc (Typ);
            Error_Msg_NE
              ("\tagged type & declared # with ghost policy `Check`",
               Prim, Typ);

            Error_Msg_Sloc := Sloc (Prim);
            Error_Msg_N
              ("\primitive subprogram & declared # with ghost policy `Ignore`",
               Prim);
         end if;
      end if;
   end Check_Ghost_Primitive;

   ----------------------------
   -- Check_Ghost_Refinement --
   ----------------------------

   procedure Check_Ghost_Refinement
     (State      : Node_Id;
      State_Id   : Entity_Id;
      Constit    : Node_Id;
      Constit_Id : Entity_Id)
   is
   begin
      if Is_Ghost_Entity (State_Id) then
         if Is_Ghost_Entity (Constit_Id) then

            --  The Ghost policy in effect at the point of abstract state
            --  declaration and constituent must match (SPARK RM 6.9(15)).

            if Is_Checked_Ghost_Entity (State_Id)
              and then Is_Ignored_Ghost_Entity (Constit_Id)
            then
               Error_Msg_Sloc := Sloc (Constit);
               SPARK_Msg_N ("incompatible ghost policies in effect", State);

               SPARK_Msg_NE
                 ("\abstract state & declared with ghost policy `Check`",
                  State, State_Id);
               SPARK_Msg_NE
                 ("\constituent & declared # with ghost policy `Ignore`",
                  State, Constit_Id);

            elsif Is_Ignored_Ghost_Entity (State_Id)
              and then Is_Checked_Ghost_Entity (Constit_Id)
            then
               Error_Msg_Sloc := Sloc (Constit);
               SPARK_Msg_N ("incompatible ghost policies in effect", State);

               SPARK_Msg_NE
                 ("\abstract state & declared with ghost policy `Ignore`",
                  State, State_Id);
               SPARK_Msg_NE
                 ("\constituent & declared # with ghost policy `Check`",
                  State, Constit_Id);
            end if;

            --  A constituent of a Ghost abstract state must be a Ghost entity
            --  (SPARK RM 7.2.2(12)).

         else
            SPARK_Msg_NE
              ("constituent of ghost state & must be ghost",
               Constit, State_Id);
         end if;
      end if;
   end Check_Ghost_Refinement;

   ------------------
   -- Ghost_Entity --
   ------------------

   function Ghost_Entity (N : Node_Id) return Entity_Id is
      Ref : Node_Id;

   begin
      --  When the reference extracts a subcomponent, recover the related
      --  object (SPARK RM 6.9(1)).

      Ref := N;
      while Nkind_In (Ref, N_Explicit_Dereference,
                           N_Indexed_Component,
                           N_Selected_Component,
                           N_Slice)
      loop
         Ref := Prefix (Ref);
      end loop;

      if Is_Entity_Name (Ref) then
         return Entity (Ref);
      else
         return Empty;
      end if;
   end Ghost_Entity;

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

         --  Determine whether the expression of the aspect or pragma is static
         --  and denotes True.

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
         --  stop the traversal. Check the original node as source constructs
         --  may be rewritten into something else by expansion.

         elsif Comes_From_Source (Original_Node (Decl)) then
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

   -----------------------------
   -- Mark_Full_View_As_Ghost --
   -----------------------------

   procedure Mark_Full_View_As_Ghost
     (Priv_Typ : Entity_Id;
      Full_Typ : Entity_Id)
   is
      Full_Decl : constant Node_Id := Declaration_Node (Full_Typ);

   begin
      if Is_Checked_Ghost_Entity (Priv_Typ) then
         Set_Is_Checked_Ghost_Entity (Full_Typ);

      elsif Is_Ignored_Ghost_Entity (Priv_Typ) then
         Set_Is_Ignored_Ghost_Entity (Full_Typ);
         Set_Is_Ignored_Ghost_Node (Full_Decl);
         Propagate_Ignored_Ghost_Code (Full_Decl);
      end if;
   end Mark_Full_View_As_Ghost;

   --------------------------
   -- Mark_Pragma_As_Ghost --
   --------------------------

   procedure Mark_Pragma_As_Ghost
     (Prag       : Node_Id;
      Context_Id : Entity_Id)
   is
   begin
      if Is_Checked_Ghost_Entity (Context_Id) then
         Set_Is_Ghost_Pragma (Prag);

      elsif Is_Ignored_Ghost_Entity (Context_Id) then
         Set_Is_Ghost_Pragma (Prag);
         Set_Is_Ignored_Ghost_Node (Prag);
         Propagate_Ignored_Ghost_Code (Prag);
      end if;
   end Mark_Pragma_As_Ghost;

   ----------------------------
   -- Mark_Renaming_As_Ghost --
   ----------------------------

   procedure Mark_Renaming_As_Ghost
     (Ren_Decl : Node_Id;
      Nam_Id   : Entity_Id)
   is
      Ren_Id : constant Entity_Id := Defining_Entity (Ren_Decl);

   begin
      if Is_Checked_Ghost_Entity (Nam_Id) then
         Set_Is_Checked_Ghost_Entity (Ren_Id);

      elsif Is_Ignored_Ghost_Entity (Nam_Id) then
         Set_Is_Ignored_Ghost_Entity (Ren_Id);
         Set_Is_Ignored_Ghost_Node (Ren_Decl);
         Propagate_Ignored_Ghost_Code (Ren_Decl);
      end if;
   end Mark_Renaming_As_Ghost;

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

   procedure Set_Ghost_Mode (N : Node_Id; Id : Entity_Id := Empty) is
      procedure Set_From_Entity (Ent_Id : Entity_Id);
      --  Set the value of global variable Ghost_Mode depending on the mode of
      --  entity Ent_Id.

      procedure Set_From_Policy;
      --  Set the value of global variable Ghost_Mode depending on the current
      --  Ghost policy in effect.

      ---------------------
      -- Set_From_Entity --
      ---------------------

      procedure Set_From_Entity (Ent_Id : Entity_Id) is
      begin
         Set_Ghost_Mode_From_Entity (Ent_Id);

         if Is_Ignored_Ghost_Entity (Ent_Id) then
            Set_Is_Ignored_Ghost_Node (N);
            Propagate_Ignored_Ghost_Code (N);
         end if;
      end Set_From_Entity;

      ---------------------
      -- Set_From_Policy --
      ---------------------

      procedure Set_From_Policy is
         Policy : constant Name_Id := Policy_In_Effect (Name_Ghost);

      begin
         if Policy = Name_Check then
            Ghost_Mode := Check;

         elsif Policy = Name_Ignore then
            Ghost_Mode := Ignore;

            Set_Is_Ignored_Ghost_Node (N);
            Propagate_Ignored_Ghost_Code (N);
         end if;
      end Set_From_Policy;

      --  Local variables

      Nam_Id : Entity_Id;

   --  Start of processing for Set_Ghost_Mode

   begin
      --  The input node denotes one of the many declaration kinds that may be
      --  subject to pragma Ghost.

      if Is_Declaration (N) then
         if Is_Subject_To_Ghost (N) then
            Set_From_Policy;

         --  The declaration denotes the completion of a deferred constant,
         --  pragma Ghost appears on the partial declaration.

         elsif Nkind (N) = N_Object_Declaration
           and then Constant_Present (N)
           and then Present (Id)
         then
            Set_From_Entity (Id);

         --  The declaration denotes the full view of a private type, pragma
         --  Ghost appears on the partial declaration.

         elsif Nkind (N) = N_Full_Type_Declaration
           and then Is_Private_Type (Defining_Entity (N))
           and then Present (Id)
         then
            Set_From_Entity (Id);
         end if;

      --  The input denotes an assignment or a procedure call. In this case
      --  the Ghost mode is dictated by the name of the construct.

      elsif Nkind_In (N, N_Assignment_Statement,
                         N_Procedure_Call_Statement)
      then
         Nam_Id := Ghost_Entity (Name (N));

         if Present (Nam_Id) then
            Set_From_Entity (Nam_Id);
         end if;

      --  The input denotes a package or subprogram body

      elsif Nkind_In (N, N_Package_Body, N_Subprogram_Body) then
         if (Present (Id) and then Is_Ghost_Entity (Id))
           or else Is_Subject_To_Ghost (N)
         then
            Set_From_Policy;
         end if;

      --  The input denotes a pragma

      elsif Nkind (N) = N_Pragma and then Is_Ghost_Pragma (N) then
         if Is_Ignored_Ghost_Node (N) then
            Ghost_Mode := Ignore;
         else
            Ghost_Mode := Check;
         end if;

      --  The input denotes a freeze node

      elsif Nkind (N) = N_Freeze_Entity and then Present (Id) then
         Set_From_Entity (Id);
      end if;
   end Set_Ghost_Mode;

   --------------------------------
   -- Set_Ghost_Mode_From_Entity --
   --------------------------------

   procedure Set_Ghost_Mode_From_Entity (Id : Entity_Id) is
   begin
      if Is_Checked_Ghost_Entity (Id) then
         Ghost_Mode := Check;
      elsif Is_Ignored_Ghost_Entity (Id) then
         Ghost_Mode := Ignore;
      end if;
   end Set_Ghost_Mode_From_Entity;

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
