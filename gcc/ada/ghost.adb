------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G H O S T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2014-2021, Free Software Foundation, Inc.         --
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

with Alloc;
with Aspects;        use Aspects;
with Atree;          use Atree;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Disp;       use Sem_Disp;
with Sem_Eval;       use Sem_Eval;
with Sem_Prag;       use Sem_Prag;
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Table;

package body Ghost is

   ---------------------
   -- Data strictures --
   ---------------------

   --  The following table contains all ignored Ghost nodes that must be
   --  eliminated from the tree by routine Remove_Ignored_Ghost_Code.

   package Ignored_Ghost_Nodes is new Table.Table (
     Table_Component_Type => Node_Id,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Ignored_Ghost_Nodes_Initial,
     Table_Increment      => Alloc.Ignored_Ghost_Nodes_Increment,
     Table_Name           => "Ignored_Ghost_Nodes");

   -----------------------
   -- Local subprograms --
   -----------------------

   function Whole_Object_Ref (Ref : Node_Id) return Node_Id;
   --  For a name that denotes an object, returns a name that denotes the whole
   --  object, declared by an object declaration, formal parameter declaration,
   --  etc. For example, for P.X.Comp (J), if P is a package X is a record
   --  object, this returns P.X.

   function Ghost_Entity (Ref : Node_Id) return Entity_Id;
   pragma Inline (Ghost_Entity);
   --  Obtain the entity of a Ghost entity from reference Ref. Return Empty if
   --  no such entity exists.

   procedure Install_Ghost_Mode (Mode : Ghost_Mode_Type);
   pragma Inline (Install_Ghost_Mode);
   --  Install Ghost mode Mode as the Ghost mode in effect

   procedure Install_Ghost_Region (Mode : Name_Id; N : Node_Id);
   pragma Inline (Install_Ghost_Region);
   --  Install a Ghost region comprised of mode Mode and ignored region start
   --  node N.

   function Is_Subject_To_Ghost (N : Node_Id) return Boolean;
   --  Determine whether declaration or body N is subject to aspect or pragma
   --  Ghost. This routine must be used in cases where pragma Ghost has not
   --  been analyzed yet, but the context needs to establish the "ghostness"
   --  of N.

   procedure Mark_Ghost_Declaration_Or_Body
     (N    : Node_Id;
      Mode : Name_Id);
   --  Mark the defining entity of declaration or body N as Ghost depending on
   --  mode Mode. Mark all formals parameters when N denotes a subprogram or a
   --  body.

   function Name_To_Ghost_Mode (Mode : Name_Id) return Ghost_Mode_Type;
   pragma Inline (Name_To_Ghost_Mode);
   --  Convert a Ghost mode denoted by name Mode into its respective enumerated
   --  value.

   procedure Record_Ignored_Ghost_Node (N : Node_Or_Entity_Id);
   --  Save ignored Ghost node or entity N in table Ignored_Ghost_Nodes for
   --  later elimination.

   ----------------------------
   -- Check_Ghost_Completion --
   ----------------------------

   procedure Check_Ghost_Completion
     (Prev_Id  : Entity_Id;
      Compl_Id : Entity_Id)
   is
      Policy : constant Name_Id := Policy_In_Effect (Name_Ghost);

   begin
      --  Nothing to do if one of the views is missing

      if No (Prev_Id) or else No (Compl_Id) then
         null;

      --  The Ghost policy in effect at the point of declaration and at the
      --  point of completion must match (SPARK RM 6.9(14)).

      elsif Is_Checked_Ghost_Entity (Prev_Id)
        and then Policy = Name_Ignore
      then
         Error_Msg_Sloc := Sloc (Compl_Id);

         Error_Msg_N ("incompatible ghost policies in effect", Prev_Id);
         Error_Msg_N ("\& declared with ghost policy `Check`", Prev_Id);
         Error_Msg_N ("\& completed # with ghost policy `Ignore`", Prev_Id);

      elsif Is_Ignored_Ghost_Entity (Prev_Id)
        and then Policy = Name_Check
      then
         Error_Msg_Sloc := Sloc (Compl_Id);

         Error_Msg_N ("incompatible ghost policies in effect", Prev_Id);
         Error_Msg_N ("\& declared with ghost policy `Ignore`", Prev_Id);
         Error_Msg_N ("\& completed # with ghost policy `Check`", Prev_Id);
      end if;
   end Check_Ghost_Completion;

   -------------------------
   -- Check_Ghost_Context --
   -------------------------

   procedure Check_Ghost_Context (Ghost_Id : Entity_Id; Ghost_Ref : Node_Id) is
      procedure Check_Ghost_Policy (Id : Entity_Id; Ref : Node_Id);
      --  Verify that the Ghost policy at the point of declaration of entity Id
      --  matches the policy at the point of reference Ref. If this is not the
      --  case emit an error at Ref.

      function Is_OK_Ghost_Context (Context : Node_Id) return Boolean;
      --  Determine whether node Context denotes a Ghost-friendly context where
      --  a Ghost entity can safely reside (SPARK RM 6.9(10)).

      function In_Aspect_Or_Pragma_Predicate (N : Node_Id) return Boolean;
      --  Return True iff N is enclosed in an aspect or pragma Predicate

      -------------------------
      -- Is_OK_Ghost_Context --
      -------------------------

      function Is_OK_Ghost_Context (Context : Node_Id) return Boolean is
         function Is_OK_Declaration (Decl : Node_Id) return Boolean;
         --  Determine whether node Decl is a suitable context for a reference
         --  to a Ghost entity. To qualify as such, Decl must either
         --
         --    * Define a Ghost entity
         --
         --    * Be subject to pragma Ghost

         function Is_OK_Pragma (Prag : Node_Id) return Boolean;
         --  Determine whether node Prag is a suitable context for a reference
         --  to a Ghost entity. To qualify as such, Prag must either
         --
         --    * Be an assertion expression pragma
         --
         --    * Denote pragma Global, Depends, Initializes, Refined_Global,
         --      Refined_Depends or Refined_State.
         --
         --    * Specify an aspect of a Ghost entity
         --
         --    * Contain a reference to a Ghost entity

         function Is_OK_Statement (Stmt : Node_Id) return Boolean;
         --  Determine whether node Stmt is a suitable context for a reference
         --  to a Ghost entity. To qualify as such, Stmt must either
         --
         --    * Denote a procedure call to a Ghost procedure
         --
         --    * Denote an assignment statement whose target is Ghost

         -----------------------
         -- Is_OK_Declaration --
         -----------------------

         function Is_OK_Declaration (Decl : Node_Id) return Boolean is
            function In_Subprogram_Body_Profile (N : Node_Id) return Boolean;
            --  Determine whether node N appears in the profile of a subprogram
            --  body.

            --------------------------------
            -- In_Subprogram_Body_Profile --
            --------------------------------

            function In_Subprogram_Body_Profile (N : Node_Id) return Boolean is
               Spec : constant Node_Id := Parent (N);

            begin
               --  The node appears in a parameter specification in which case
               --  it is either the parameter type or the default expression or
               --  the node appears as the result definition of a function.

               return
                 (Nkind (N) = N_Parameter_Specification
                   or else
                     (Nkind (Spec) = N_Function_Specification
                       and then N = Result_Definition (Spec)))
                   and then Nkind (Parent (Spec)) = N_Subprogram_Body;
            end In_Subprogram_Body_Profile;

            --  Local variables

            Subp_Decl : Node_Id;
            Subp_Id   : Entity_Id;

         --  Start of processing for Is_OK_Declaration

         begin
            if Is_Ghost_Declaration (Decl) then
               return True;

            --  Special cases

            --  A reference to a Ghost entity may appear within the profile of
            --  a subprogram body. This context is treated as suitable because
            --  it duplicates the context of the corresponding spec. The real
            --  check was already performed during the analysis of the spec.

            elsif In_Subprogram_Body_Profile (Decl) then
               return True;

            --  A reference to a Ghost entity may appear within an expression
            --  function which is still being analyzed. This context is treated
            --  as suitable because it is not yet known whether the expression
            --  function is an initial declaration or a completion. The real
            --  check is performed when the expression function is expanded.

            elsif Nkind (Decl) = N_Expression_Function
              and then not Analyzed (Decl)
            then
               return True;

            --  References to Ghost entities may be relocated in internally
            --  generated bodies.

            elsif Nkind (Decl) = N_Subprogram_Body
              and then not Comes_From_Source (Decl)
            then
               Subp_Id := Corresponding_Spec (Decl);

               if Present (Subp_Id) then

                  --  The context is the internally built _Postconditions
                  --  procedure, which is OK because the real check was done
                  --  before expansion activities.

                  if Chars (Subp_Id) = Name_uPostconditions then
                     return True;

                  --  The context is the internally built predicate function,
                  --  which is OK because the real check was done before the
                  --  predicate function was generated.

                  elsif Is_Predicate_Function (Subp_Id) then
                     return True;

                  else
                     Subp_Decl :=
                       Original_Node (Unit_Declaration_Node (Subp_Id));

                     --  The original context is an expression function that
                     --  has been split into a spec and a body. The context is
                     --  OK as long as the initial declaration is Ghost.

                     if Nkind (Subp_Decl) = N_Expression_Function then
                        return Is_Ghost_Declaration (Subp_Decl);
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
               --  reference to a Ghost entity (SPARK RM 6.9(10)), except for
               --  predicate pragmas (SPARK RM 6.9(11)).

               elsif Assertion_Expression_Pragma (Prag_Id)
                 and then Prag_Id /= Pragma_Predicate
               then
                  --  Ensure that the assertion policy and the Ghost policy are
                  --  compatible (SPARK RM 6.9(18)).

                  Check_Policies (Prag_Nam);
                  return True;

               --  Several pragmas that may apply to a non-Ghost entity are
               --  treated as Ghost when they contain a reference to a Ghost
               --  entity (SPARK RM 6.9(11)).

               elsif Prag_Nam in Name_Global
                               | Name_Depends
                               | Name_Initializes
                               | Name_Refined_Global
                               | Name_Refined_Depends
                               | Name_Refined_State
               then
                  return True;
               end if;
            end if;

            return False;
         end Is_OK_Pragma;

         ---------------------
         -- Is_OK_Statement --
         ---------------------

         function Is_OK_Statement (Stmt : Node_Id) return Boolean is
         begin
            --  An assignment statement is Ghost when the target is a Ghost
            --  entity.

            if Nkind (Stmt) = N_Assignment_Statement then
               return Is_Ghost_Assignment (Stmt);

            --  A procedure call is Ghost when it calls a Ghost procedure

            elsif Nkind (Stmt) = N_Procedure_Call_Statement then
               return Is_Ghost_Procedure_Call (Stmt);

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

         --  A Ghost type may be referenced in a use_type clause
         --  (SPARK RM 6.9.10).

         elsif Present (Parent (Context))
           and then Nkind (Parent (Context)) = N_Use_Type_Clause
         then
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
               --  specification (SPARK RM 6.9(10)). The precise checking will
               --  occur when analyzing the corresponding pragma. We make an
               --  exception for predicate aspects that only allow referencing
               --  a Ghost entity when the corresponding type declaration is
               --  Ghost (SPARK RM 6.9(11)).

               elsif Nkind (Par) = N_Aspect_Specification
                 and then not Same_Aspect
                                (Get_Aspect_Id (Par), Aspect_Predicate)
               then
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

      procedure Check_Ghost_Policy (Id : Entity_Id; Ref : Node_Id) is
         Policy : constant Name_Id := Policy_In_Effect (Name_Ghost);

      begin
         --  The Ghost policy in effect a the point of declaration and at the
         --  point of use must match (SPARK RM 6.9(13)).

         if Is_Checked_Ghost_Entity (Id)
           and then Policy = Name_Ignore
           and then May_Be_Lvalue (Ref)
         then
            Error_Msg_Sloc := Sloc (Ref);

            Error_Msg_N  ("incompatible ghost policies in effect", Ref);
            Error_Msg_NE ("\& declared with ghost policy `Check`", Ref, Id);
            Error_Msg_NE ("\& used # with ghost policy `Ignore`",  Ref, Id);

         elsif Is_Ignored_Ghost_Entity (Id) and then Policy = Name_Check then
            Error_Msg_Sloc := Sloc (Ref);

            Error_Msg_N  ("incompatible ghost policies in effect",  Ref);
            Error_Msg_NE ("\& declared with ghost policy `Ignore`", Ref, Id);
            Error_Msg_NE ("\& used # with ghost policy `Check`",    Ref, Id);
         end if;
      end Check_Ghost_Policy;

      -----------------------------------
      -- In_Aspect_Or_Pragma_Predicate --
      -----------------------------------

      function In_Aspect_Or_Pragma_Predicate (N : Node_Id) return Boolean is
         Par : Node_Id := N;
      begin
         while Present (Par) loop
            if Nkind (Par) = N_Pragma
              and then Get_Pragma_Id (Par) = Pragma_Predicate
            then
               return True;

            elsif Nkind (Par) = N_Aspect_Specification
              and then Same_Aspect (Get_Aspect_Id (Par), Aspect_Predicate)
            then
               return True;

            --  Stop the search when it's clear it cannot be inside an aspect
            --  or pragma.

            elsif Is_Declaration (Par)
              or else Is_Statement (Par)
              or else Is_Body (Par)
            then
               return False;
            end if;

            Par := Parent (Par);
         end loop;

         return False;
      end In_Aspect_Or_Pragma_Predicate;

   --  Start of processing for Check_Ghost_Context

   begin
      --  Once it has been established that the reference to the Ghost entity
      --  is within a suitable context, ensure that the policy at the point of
      --  declaration and at the point of use match.

      if Is_OK_Ghost_Context (Ghost_Ref) then
         Check_Ghost_Policy (Ghost_Id, Ghost_Ref);

      --  Otherwise the Ghost entity appears in a non-Ghost context and affects
      --  its behavior or value (SPARK RM 6.9(10,11)).

      else
         Error_Msg_N ("ghost entity cannot appear in this context", Ghost_Ref);

         --  When the Ghost entity appears in a pragma Predicate, explain the
         --  reason for this being illegal, and suggest a fix instead.

         if In_Aspect_Or_Pragma_Predicate (Ghost_Ref) then
            Error_Msg_N
              ("\as predicates are checked in membership tests, "
               & "the type and its predicate must be both ghost",
               Ghost_Ref);
            Error_Msg_N
              ("\either make the type ghost "
               & "or use a type invariant on a private type", Ghost_Ref);
         end if;
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
           and then not Is_Abstract_Subprogram (Over_Subp)
         then
            Error_Msg_N ("incompatible overriding in effect", Subp);

            Error_Msg_Sloc := Sloc (Over_Subp);
            Error_Msg_N ("\& declared # as non-ghost subprogram", Subp);

            Error_Msg_Sloc := Sloc (Subp);
            Error_Msg_N ("\overridden # with ghost subprogram", Subp);
         end if;

         --  A non-Ghost primitive of a type extension cannot override an
         --  inherited Ghost primitive (SPARK RM 6.9(8)).

         if Is_Ghost_Entity (Over_Subp)
           and then not Is_Ghost_Entity (Subp)
           and then not Is_Abstract_Subprogram (Subp)
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

   ----------------------
   -- Check_Ghost_Type --
   ----------------------

   procedure Check_Ghost_Type (Typ : Entity_Id) is
      Conc_Typ : Entity_Id;
      Full_Typ : Entity_Id;

   begin
      if Is_Ghost_Entity (Typ) then
         Conc_Typ := Empty;
         Full_Typ := Typ;

         if Is_Single_Concurrent_Type (Typ) then
            Conc_Typ := Anonymous_Object (Typ);
            Full_Typ := Conc_Typ;

         elsif Is_Concurrent_Type (Typ) then
            Conc_Typ := Typ;
         end if;

         --  A Ghost type cannot be concurrent (SPARK RM 6.9(19)). Verify this
         --  legality rule first to give a finer-grained diagnostic.

         if Present (Conc_Typ) then
            Error_Msg_N ("ghost type & cannot be concurrent", Conc_Typ);
         end if;

         --  A Ghost type cannot be effectively volatile (SPARK RM 6.9(7))

         if Is_Effectively_Volatile (Full_Typ) then
            Error_Msg_N ("ghost type & cannot be volatile", Full_Typ);
         end if;
      end if;
   end Check_Ghost_Type;

   ------------------
   -- Ghost_Entity --
   ------------------

   function Ghost_Entity (Ref : Node_Id) return Entity_Id is
      Obj_Ref : constant Node_Id := Ultimate_Prefix (Ref);

   begin
      --  When the reference denotes a subcomponent, recover the related whole
      --  object (SPARK RM 6.9(1)).

      if Is_Entity_Name (Obj_Ref) then
         return Entity (Obj_Ref);

      --  Otherwise the reference cannot possibly denote a Ghost entity

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
      Ignored_Ghost_Nodes.Init;

      --  Set the soft link which enables Atree.Mark_New_Ghost_Node to record
      --  an ignored Ghost node or entity.

      Set_Ignored_Ghost_Recording_Proc (Record_Ignored_Ghost_Node'Access);
   end Initialize;

   ------------------------
   -- Install_Ghost_Mode --
   ------------------------

   procedure Install_Ghost_Mode (Mode : Ghost_Mode_Type) is
   begin
      Install_Ghost_Region (Mode, Empty);
   end Install_Ghost_Mode;

   --------------------------
   -- Install_Ghost_Region --
   --------------------------

   procedure Install_Ghost_Region (Mode : Ghost_Mode_Type; N : Node_Id) is
   begin
      --  The context is already within an ignored Ghost region. Maintain the
      --  start of the outermost ignored Ghost region.

      if Present (Ignored_Ghost_Region) then
         null;

      --  The current region is the outermost ignored Ghost region. Save its
      --  starting node.

      elsif Present (N) and then Mode = Ignore then
         Ignored_Ghost_Region := N;

      --  Otherwise the current region is not ignored, nothing to save

      else
         Ignored_Ghost_Region := Empty;
      end if;

      Ghost_Mode := Mode;
   end Install_Ghost_Region;

   procedure Install_Ghost_Region (Mode : Name_Id; N : Node_Id) is
   begin
      Install_Ghost_Region (Name_To_Ghost_Mode (Mode), N);
   end Install_Ghost_Region;

   -------------------------
   -- Is_Ghost_Assignment --
   -------------------------

   function Is_Ghost_Assignment (N : Node_Id) return Boolean is
      Id : Entity_Id;

   begin
      --  An assignment statement is Ghost when its target denotes a Ghost
      --  entity.

      if Nkind (N) = N_Assignment_Statement then
         Id := Ghost_Entity (Name (N));

         return Present (Id) and then Is_Ghost_Entity (Id);
      end if;

      return False;
   end Is_Ghost_Assignment;

   --------------------------
   -- Is_Ghost_Declaration --
   --------------------------

   function Is_Ghost_Declaration (N : Node_Id) return Boolean is
      Id : Entity_Id;

   begin
      --  A declaration is Ghost when it elaborates a Ghost entity or is
      --  subject to pragma Ghost.

      if Is_Declaration (N) then
         Id := Defining_Entity (N);

         return Is_Ghost_Entity (Id) or else Is_Subject_To_Ghost (N);
      end if;

      return False;
   end Is_Ghost_Declaration;

   ---------------------
   -- Is_Ghost_Pragma --
   ---------------------

   function Is_Ghost_Pragma (N : Node_Id) return Boolean is
   begin
      return Is_Checked_Ghost_Pragma (N) or else Is_Ignored_Ghost_Pragma (N);
   end Is_Ghost_Pragma;

   -----------------------------
   -- Is_Ghost_Procedure_Call --
   -----------------------------

   function Is_Ghost_Procedure_Call (N : Node_Id) return Boolean is
      Id : Entity_Id;

   begin
      --  A procedure call is Ghost when it invokes a Ghost procedure

      if Nkind (N) = N_Procedure_Call_Statement then
         Id := Ghost_Entity (Name (N));

         return Present (Id) and then Is_Ghost_Entity (Id);
      end if;

      return False;
   end Is_Ghost_Procedure_Call;

   ---------------------------
   -- Is_Ignored_Ghost_Unit --
   ---------------------------

   function Is_Ignored_Ghost_Unit (N : Node_Id) return Boolean is
      function Ultimate_Original_Node (Nod : Node_Id) return Node_Id;
      --  Obtain the original node of arbitrary node Nod following a potential
      --  chain of rewritings.

      ----------------------------
      -- Ultimate_Original_Node --
      ----------------------------

      function Ultimate_Original_Node (Nod : Node_Id) return Node_Id is
         Res : Node_Id := Nod;
      begin
         while Original_Node (Res) /= Res loop
            Res := Original_Node (Res);
         end loop;

         return Res;
      end Ultimate_Original_Node;

   --  Start of processing for Is_Ignored_Ghost_Unit

   begin
      --  Inspect the original node of the unit in case removal of ignored
      --  Ghost code has already taken place.

      return
        Nkind (N) = N_Compilation_Unit
          and then Is_Ignored_Ghost_Entity
                     (Defining_Entity (Ultimate_Original_Node (Unit (N))));
   end Is_Ignored_Ghost_Unit;

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

      if Nkind (N) in N_Generic_Package_Declaration | N_Package_Declaration
      then
         Decl := First (Visible_Declarations (Specification (N)));

      --  When the context is a package or a subprogram body, pragma Ghost
      --  resides in the declarative part.

      elsif Nkind (N) in N_Package_Body | N_Subprogram_Body then
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
      Ignored_Ghost_Nodes.Release;
      Ignored_Ghost_Nodes.Locked := True;
   end Lock;

   -----------------------------------
   -- Mark_And_Set_Ghost_Assignment --
   -----------------------------------

   procedure Mark_And_Set_Ghost_Assignment (N : Node_Id) is
      --  A ghost assignment is an assignment whose left-hand side denotes a
      --  ghost object. Subcomponents are not marked "ghost", so we need to
      --  find the containing "whole" object. So, for "P.X.Comp (J) := ...",
      --  where P is a package, X is a record, and Comp is an array, we need
      --  to check the ghost flags of X.

      Orig_Lhs : constant Node_Id := Name (N);
   begin
      --  Ghost assignments are irrelevant when the expander is inactive, and
      --  processing them in that mode can lead to spurious errors.

      if Expander_Active then
         if not Analyzed (Orig_Lhs)
           and then Nkind (Orig_Lhs) = N_Indexed_Component
           and then Nkind (Prefix (Orig_Lhs)) = N_Selected_Component
           and then Nkind (Prefix (Prefix (Orig_Lhs))) =
           N_Indexed_Component
         then
            Analyze (Orig_Lhs);
         end if;

         --  Make sure Lhs is at least preanalyzed, so we can tell whether
         --  it denotes a ghost variable. In some cases we need to do a full
         --  analysis, or else the back end gets confused. Note that in the
         --  preanalysis case, we are preanalyzing a copy of the left-hand
         --  side name, temporarily attached to the tree.

         declare
            Lhs : constant Node_Id :=
              (if Analyzed (Orig_Lhs) then Orig_Lhs
               else New_Copy_Tree (Orig_Lhs));
         begin
            if not Analyzed (Lhs) then
               Set_Name   (N, Lhs);
               Set_Parent (Lhs, N);
               Preanalyze_Without_Errors (Lhs);
               Set_Name (N, Orig_Lhs);
            end if;

            declare
               Whole : constant Node_Id := Whole_Object_Ref (Lhs);
               Id    : Entity_Id;
            begin
               if Is_Entity_Name (Whole) then
                  Id := Entity (Whole);

                  if Present (Id) then
                     --  Left-hand side denotes a Checked ghost entity, so
                     --  install the region.

                     if Is_Checked_Ghost_Entity (Id) then
                        Install_Ghost_Region (Check, N);

                     --  Left-hand side denotes an Ignored ghost entity, so
                     --  install the region, and mark the assignment statement
                     --  as an ignored ghost assignment, so it will be removed
                     --  later.

                     elsif Is_Ignored_Ghost_Entity (Id) then
                        Install_Ghost_Region (Ignore, N);
                        Set_Is_Ignored_Ghost_Node (N);
                        Record_Ignored_Ghost_Node (N);
                     end if;
                  end if;
               end if;
            end;
         end;
      end if;
   end Mark_And_Set_Ghost_Assignment;

   -----------------------------
   -- Mark_And_Set_Ghost_Body --
   -----------------------------

   procedure Mark_And_Set_Ghost_Body
     (N       : Node_Id;
      Spec_Id : Entity_Id)
   is
      Body_Id : constant Entity_Id := Defining_Entity (N);
      Policy  : Name_Id := No_Name;

   begin
      --  A body becomes Ghost when it is subject to aspect or pragma Ghost

      if Is_Subject_To_Ghost (N) then
         Policy := Policy_In_Effect (Name_Ghost);

      --  A body declared within a Ghost region is automatically Ghost
      --  (SPARK RM 6.9(2)).

      elsif Ghost_Mode = Check then
         Policy := Name_Check;

      elsif Ghost_Mode = Ignore then
         Policy := Name_Ignore;

      --  Inherit the "ghostness" of the previous declaration when the body
      --  acts as a completion.

      elsif Present (Spec_Id) then
         if Is_Checked_Ghost_Entity (Spec_Id) then
            Policy := Name_Check;

         elsif Is_Ignored_Ghost_Entity (Spec_Id) then
            Policy := Name_Ignore;
         end if;
      end if;

      --  The Ghost policy in effect at the point of declaration and at the
      --  point of completion must match (SPARK RM 6.9(14)).

      Check_Ghost_Completion
        (Prev_Id  => Spec_Id,
         Compl_Id => Body_Id);

      --  Mark the body as its formals as Ghost

      Mark_Ghost_Declaration_Or_Body (N, Policy);

      --  Install the appropriate Ghost region

      Install_Ghost_Region (Policy, N);
   end Mark_And_Set_Ghost_Body;

   -----------------------------------
   -- Mark_And_Set_Ghost_Completion --
   -----------------------------------

   procedure Mark_And_Set_Ghost_Completion
     (N       : Node_Id;
      Prev_Id : Entity_Id)
   is
      Compl_Id : constant Entity_Id := Defining_Entity (N);
      Policy   : Name_Id := No_Name;

   begin
      --  A completion elaborated in a Ghost region is automatically Ghost
      --  (SPARK RM 6.9(2)).

      if Ghost_Mode = Check then
         Policy := Name_Check;

      elsif Ghost_Mode = Ignore then
         Policy := Name_Ignore;

      --  The completion becomes Ghost when its initial declaration is also
      --  Ghost.

      elsif Is_Checked_Ghost_Entity (Prev_Id) then
         Policy := Name_Check;

      elsif Is_Ignored_Ghost_Entity (Prev_Id) then
         Policy := Name_Ignore;
      end if;

      --  The Ghost policy in effect at the point of declaration and at the
      --  point of completion must match (SPARK RM 6.9(14)).

      Check_Ghost_Completion
        (Prev_Id  => Prev_Id,
         Compl_Id => Compl_Id);

      --  Mark the completion as Ghost

      Mark_Ghost_Declaration_Or_Body (N, Policy);

      --  Install the appropriate Ghost region

      Install_Ghost_Region (Policy, N);
   end Mark_And_Set_Ghost_Completion;

   ------------------------------------
   -- Mark_And_Set_Ghost_Declaration --
   ------------------------------------

   procedure Mark_And_Set_Ghost_Declaration (N : Node_Id) is
      Par_Id : Entity_Id;
      Policy : Name_Id := No_Name;

   begin
      --  A declaration becomes Ghost when it is subject to aspect or pragma
      --  Ghost.

      if Is_Subject_To_Ghost (N) then
         Policy := Policy_In_Effect (Name_Ghost);

      --  A declaration elaborated in a Ghost region is automatically Ghost
      --  (SPARK RM 6.9(2)).

      elsif Ghost_Mode = Check then
         Policy := Name_Check;

      elsif Ghost_Mode = Ignore then
         Policy := Name_Ignore;

      --  A child package or subprogram declaration becomes Ghost when its
      --  parent is Ghost (SPARK RM 6.9(2)).

      elsif Nkind (N) in N_Generic_Function_Renaming_Declaration
                       | N_Generic_Package_Declaration
                       | N_Generic_Package_Renaming_Declaration
                       | N_Generic_Procedure_Renaming_Declaration
                       | N_Generic_Subprogram_Declaration
                       | N_Package_Declaration
                       | N_Package_Renaming_Declaration
                       | N_Subprogram_Declaration
                       | N_Subprogram_Renaming_Declaration
        and then Present (Parent_Spec (N))
      then
         Par_Id := Defining_Entity (Unit (Parent_Spec (N)));

         if Is_Checked_Ghost_Entity (Par_Id) then
            Policy := Name_Check;

         elsif Is_Ignored_Ghost_Entity (Par_Id) then
            Policy := Name_Ignore;
         end if;
      end if;

      --  Mark the declaration and its formals as Ghost

      Mark_Ghost_Declaration_Or_Body (N, Policy);

      --  Install the appropriate Ghost region

      Install_Ghost_Region (Policy, N);
   end Mark_And_Set_Ghost_Declaration;

   --------------------------------------
   -- Mark_And_Set_Ghost_Instantiation --
   --------------------------------------

   procedure Mark_And_Set_Ghost_Instantiation
     (N      : Node_Id;
      Gen_Id : Entity_Id)
   is
      procedure Check_Ghost_Actuals;
      --  Check the context of ghost actuals

      -------------------------
      -- Check_Ghost_Actuals --
      -------------------------

      procedure Check_Ghost_Actuals is
         Assoc : Node_Id := First (Generic_Associations (N));
         Act   : Node_Id;

      begin
         while Present (Assoc) loop
            if Nkind (Assoc) /= N_Others_Choice then
               Act := Explicit_Generic_Actual_Parameter (Assoc);

               --  Within a nested instantiation, a defaulted actual is an
               --  empty association, so nothing to check.

               if No (Act) then
                  null;

               elsif Comes_From_Source (Act)
                  and then Nkind (Act) in N_Has_Etype
                  and then Present (Etype (Act))
                  and then Is_Ghost_Entity (Etype (Act))
               then
                  Check_Ghost_Context (Etype (Act), Act);
               end if;
            end if;

            Next (Assoc);
         end loop;
      end Check_Ghost_Actuals;

      --  Local variables

      Policy : Name_Id := No_Name;

   begin
      --  An instantiation becomes Ghost when it is subject to pragma Ghost

      if Is_Subject_To_Ghost (N) then
         Policy := Policy_In_Effect (Name_Ghost);

      --  An instantiation declaration within a Ghost region is automatically
      --  Ghost (SPARK RM 6.9(2)).

      elsif Ghost_Mode = Check then
         Policy := Name_Check;

      elsif Ghost_Mode = Ignore then
         Policy := Name_Ignore;

      --  Inherit the "ghostness" of the generic unit

      elsif Is_Checked_Ghost_Entity (Gen_Id) then
         Policy := Name_Check;

      elsif Is_Ignored_Ghost_Entity (Gen_Id) then
         Policy := Name_Ignore;
      end if;

      --  Mark the instantiation as Ghost

      Mark_Ghost_Declaration_Or_Body (N, Policy);

      --  Install the appropriate Ghost region

      Install_Ghost_Region (Policy, N);

      --  Check Ghost actuals. Given that this routine is unconditionally
      --  invoked with subprogram and package instantiations, this check
      --  verifies the context of all the ghost entities passed in generic
      --  instantiations.

      Check_Ghost_Actuals;
   end Mark_And_Set_Ghost_Instantiation;

   ---------------------------------------
   -- Mark_And_Set_Ghost_Procedure_Call --
   ---------------------------------------

   procedure Mark_And_Set_Ghost_Procedure_Call (N : Node_Id) is
      Id : Entity_Id;

   begin
      --  A procedure call becomes Ghost when the procedure being invoked is
      --  Ghost. Install the Ghost mode of the procedure.

      Id := Ghost_Entity (Name (N));

      if Present (Id) then
         if Is_Checked_Ghost_Entity (Id) then
            Install_Ghost_Region (Check, N);

         elsif Is_Ignored_Ghost_Entity (Id) then
            Install_Ghost_Region (Ignore, N);

            Set_Is_Ignored_Ghost_Node (N);
            Record_Ignored_Ghost_Node (N);
         end if;
      end if;
   end Mark_And_Set_Ghost_Procedure_Call;

   -----------------------
   -- Mark_Ghost_Clause --
   -----------------------

   procedure Mark_Ghost_Clause (N : Node_Id) is
      Nam : Node_Id := Empty;

   begin
      if Nkind (N) = N_Use_Package_Clause then
         Nam := Name (N);

      elsif Nkind (N) = N_Use_Type_Clause then
         Nam := Subtype_Mark (N);

      elsif Nkind (N) = N_With_Clause then
         Nam := Name (N);
      end if;

      if Present (Nam)
        and then Is_Entity_Name (Nam)
        and then Present (Entity (Nam))
        and then Is_Ignored_Ghost_Entity (Entity (Nam))
      then
         Set_Is_Ignored_Ghost_Node (N);
         Record_Ignored_Ghost_Node (N);
      end if;
   end Mark_Ghost_Clause;

   ------------------------------------
   -- Mark_Ghost_Declaration_Or_Body --
   ------------------------------------

   procedure Mark_Ghost_Declaration_Or_Body
     (N    : Node_Id;
      Mode : Name_Id)
   is
      Id : constant Entity_Id := Defining_Entity (N);

      Mark_Formals : Boolean := False;
      Param        : Node_Id;
      Param_Id     : Entity_Id;

   begin
      --  Mark the related node and its entity

      if Mode = Name_Check then
         Mark_Formals := True;
         Set_Is_Checked_Ghost_Entity (Id);

      elsif Mode = Name_Ignore then
         Mark_Formals := True;
         Set_Is_Ignored_Ghost_Entity (Id);
         Set_Is_Ignored_Ghost_Node (N);
         Record_Ignored_Ghost_Node (N);
      end if;

      --  Mark all formal parameters when the related node denotes a subprogram
      --  or a body. The traversal is performed via the specification because
      --  the related subprogram or body may be unanalyzed.

      --  ??? could extra formal parameters cause a Ghost leak?

      if Mark_Formals
        and then Nkind (N) in N_Abstract_Subprogram_Declaration
                            | N_Formal_Abstract_Subprogram_Declaration
                            | N_Formal_Concrete_Subprogram_Declaration
                            | N_Generic_Subprogram_Declaration
                            | N_Subprogram_Body
                            | N_Subprogram_Body_Stub
                            | N_Subprogram_Declaration
                            | N_Subprogram_Renaming_Declaration
      then
         Param := First (Parameter_Specifications (Specification (N)));
         while Present (Param) loop
            Param_Id := Defining_Entity (Param);

            if Mode = Name_Check then
               Set_Is_Checked_Ghost_Entity (Param_Id);

            elsif Mode = Name_Ignore then
               Set_Is_Ignored_Ghost_Entity (Param_Id);
            end if;

            Next (Param);
         end loop;
      end if;
   end Mark_Ghost_Declaration_Or_Body;

   -----------------------
   -- Mark_Ghost_Pragma --
   -----------------------

   procedure Mark_Ghost_Pragma
     (N  : Node_Id;
      Id : Entity_Id)
   is
   begin
      --  A pragma becomes Ghost when it encloses a Ghost entity or relates to
      --  a Ghost entity.

      if Is_Checked_Ghost_Entity (Id) then
         Set_Is_Checked_Ghost_Pragma (N);

      elsif Is_Ignored_Ghost_Entity (Id) then
         Set_Is_Ignored_Ghost_Pragma (N);
         Set_Is_Ignored_Ghost_Node (N);
         Record_Ignored_Ghost_Node (N);
      end if;
   end Mark_Ghost_Pragma;

   -------------------------
   -- Mark_Ghost_Renaming --
   -------------------------

   procedure Mark_Ghost_Renaming
     (N  : Node_Id;
      Id : Entity_Id)
   is
      Policy : Name_Id := No_Name;

   begin
      --  A renaming becomes Ghost when it renames a Ghost entity

      if Is_Checked_Ghost_Entity (Id) then
         Policy := Name_Check;

      elsif Is_Ignored_Ghost_Entity (Id) then
         Policy := Name_Ignore;
      end if;

      Mark_Ghost_Declaration_Or_Body (N, Policy);
   end Mark_Ghost_Renaming;

   ------------------------
   -- Name_To_Ghost_Mode --
   ------------------------

   function Name_To_Ghost_Mode (Mode : Name_Id) return Ghost_Mode_Type is
   begin
      if Mode = Name_Check then
         return Check;

      elsif Mode = Name_Ignore then
         return Ignore;

      --  Otherwise the mode must denote one of the following:
      --
      --    * Disable indicates that the Ghost policy in effect is Disable
      --
      --    * None or No_Name indicates that the associated construct is not
      --      subject to any Ghost annotation.

      else
         pragma Assert (Mode in Name_Disable | Name_None | No_Name);
         return None;
      end if;
   end Name_To_Ghost_Mode;

   -------------------------------
   -- Record_Ignored_Ghost_Node --
   -------------------------------

   procedure Record_Ignored_Ghost_Node (N : Node_Or_Entity_Id) is
   begin
      --  Save all "top level" ignored Ghost nodes which can be safely replaced
      --  with a null statement. Note that there is need to save other kinds of
      --  nodes because those will always be enclosed by some top level ignored
      --  Ghost node.

      if Is_Body (N)
        or else Is_Declaration (N)
        or else Nkind (N) in N_Generic_Instantiation
                           | N_Push_Pop_xxx_Label
                           | N_Raise_xxx_Error
                           | N_Representation_Clause
                           | N_Statement_Other_Than_Procedure_Call
                           | N_Call_Marker
                           | N_Freeze_Entity
                           | N_Freeze_Generic_Entity
                           | N_Itype_Reference
                           | N_Pragma
                           | N_Procedure_Call_Statement
                           | N_Use_Package_Clause
                           | N_Use_Type_Clause
                           | N_Variable_Reference_Marker
                           | N_With_Clause
      then
         --  Only ignored Ghost nodes must be recorded in the table

         pragma Assert (Is_Ignored_Ghost_Node (N));
         Ignored_Ghost_Nodes.Append (N);
      end if;
   end Record_Ignored_Ghost_Node;

   -------------------------------
   -- Remove_Ignored_Ghost_Code --
   -------------------------------

   procedure Remove_Ignored_Ghost_Code is
      procedure Remove_Ignored_Ghost_Node (N : Node_Id);
      --  Eliminate ignored Ghost node N from the tree

      -------------------------------
      -- Remove_Ignored_Ghost_Node --
      -------------------------------

      procedure Remove_Ignored_Ghost_Node (N : Node_Id) is
      begin
         --  The generation and processing of ignored Ghost nodes may cause the
         --  same node to be saved multiple times. Reducing the number of saves
         --  to one involves costly solutions such as a hash table or the use
         --  of a flag shared by all nodes. To solve this problem, the removal
         --  machinery allows for multiple saves, but does not eliminate a node
         --  which has already been eliminated.

         if Nkind (N) = N_Null_Statement then
            null;

         --  Otherwise the ignored Ghost node must be eliminated

         else
            --  Only ignored Ghost nodes must be eliminated from the tree

            pragma Assert (Is_Ignored_Ghost_Node (N));

            --  Eliminate the node by rewriting it into null. Another option
            --  is to remove it from the tree, however multiple corner cases
            --  emerge which have be dealt individually.

            Rewrite (N, Make_Null_Statement (Sloc (N)));

            --  Eliminate any aspects hanging off the ignored Ghost node

            Remove_Aspects (N);
         end if;
      end Remove_Ignored_Ghost_Node;

   --  Start of processing for Remove_Ignored_Ghost_Code

   begin
      for Index in Ignored_Ghost_Nodes.First .. Ignored_Ghost_Nodes.Last loop
         Remove_Ignored_Ghost_Node (Ignored_Ghost_Nodes.Table (Index));
      end loop;
   end Remove_Ignored_Ghost_Code;

   --------------------------
   -- Restore_Ghost_Region --
   --------------------------

   procedure Restore_Ghost_Region (Mode : Ghost_Mode_Type; N : Node_Id) is
   begin
      Ghost_Mode           := Mode;
      Ignored_Ghost_Region := N;
   end Restore_Ghost_Region;

   --------------------
   -- Set_Ghost_Mode --
   --------------------

   procedure Set_Ghost_Mode (N : Node_Or_Entity_Id) is
      procedure Set_Ghost_Mode_From_Entity (Id : Entity_Id);
      --  Install the Ghost mode of entity Id

      --------------------------------
      -- Set_Ghost_Mode_From_Entity --
      --------------------------------

      procedure Set_Ghost_Mode_From_Entity (Id : Entity_Id) is
      begin
         if Is_Checked_Ghost_Entity (Id) then
            Install_Ghost_Mode (Check);
         elsif Is_Ignored_Ghost_Entity (Id) then
            Install_Ghost_Mode (Ignore);
         else
            Install_Ghost_Mode (None);
         end if;
      end Set_Ghost_Mode_From_Entity;

      --  Local variables

      Id : Entity_Id;

   --  Start of processing for Set_Ghost_Mode

   begin
      --  The Ghost mode of an assignment statement depends on the Ghost mode
      --  of the target.

      if Nkind (N) = N_Assignment_Statement then
         Id := Ghost_Entity (Name (N));

         if Present (Id) then
            Set_Ghost_Mode_From_Entity (Id);
         end if;

      --  The Ghost mode of a body or a declaration depends on the Ghost mode
      --  of its defining entity.

      elsif Is_Body (N) or else Is_Declaration (N) then
         Set_Ghost_Mode_From_Entity (Defining_Entity (N));

      --  The Ghost mode of an entity depends on the entity itself

      elsif Nkind (N) in N_Entity then
         Set_Ghost_Mode_From_Entity (N);

      --  The Ghost mode of a [generic] freeze node depends on the Ghost mode
      --  of the entity being frozen.

      elsif Nkind (N) in N_Freeze_Entity | N_Freeze_Generic_Entity then
         Set_Ghost_Mode_From_Entity (Entity (N));

      --  The Ghost mode of a pragma depends on the associated entity. The
      --  property is encoded in the pragma itself.

      elsif Nkind (N) = N_Pragma then
         if Is_Checked_Ghost_Pragma (N) then
            Install_Ghost_Mode (Check);
         elsif Is_Ignored_Ghost_Pragma (N) then
            Install_Ghost_Mode (Ignore);
         else
            Install_Ghost_Mode (None);
         end if;

      --  The Ghost mode of a procedure call depends on the Ghost mode of the
      --  procedure being invoked.

      elsif Nkind (N) = N_Procedure_Call_Statement then
         Id := Ghost_Entity (Name (N));

         if Present (Id) then
            Set_Ghost_Mode_From_Entity (Id);
         end if;
      end if;
   end Set_Ghost_Mode;

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

   ----------------------
   -- Whole_Object_Ref --
   ----------------------

   function Whole_Object_Ref (Ref : Node_Id) return Node_Id is
   begin
      if Nkind (Ref) in N_Indexed_Component | N_Slice
        or else (Nkind (Ref) = N_Selected_Component
                   and then Is_Object_Reference (Prefix (Ref)))
      then
         if Is_Access_Type (Etype (Prefix (Ref))) then
            return Ref;
         else
            return Whole_Object_Ref (Prefix (Ref));
         end if;
      else
         return Ref;
      end if;
   end Whole_Object_Ref;

end Ghost;
