------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G H O S T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2014-2017, Free Software Foundation, Inc.         --
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
   --  Find the entity of a reference to a Ghost entity. Return Empty if there
   --  is no such entity.

   procedure Install_Ghost_Mode (Mode : Name_Id);
   --  Install a specific Ghost mode denoted by Mode by setting global variable
   --  Ghost_Mode.

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

   procedure Propagate_Ignored_Ghost_Code (N : Node_Id);
   --  Signal all enclosing scopes that they now contain at least one ignored
   --  Ghost node denoted by N. Add the compilation unit containing N to table
   --  Ignored_Ghost_Units for post processing.

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

               elsif Nam_In (Prag_Nam, Name_Global,
                                       Name_Depends,
                                       Name_Initializes,
                                       Name_Refined_Global,
                                       Name_Refined_Depends,
                                       Name_Refined_State)
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

   ------------------
   -- Ghost_Entity --
   ------------------

   function Ghost_Entity (N : Node_Id) return Entity_Id is
      Ref : Node_Id;

   begin
      --  When the reference denotes a subcomponent, recover the related
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

   ------------------------
   -- Install_Ghost_Mode --
   ------------------------

   procedure Install_Ghost_Mode (Mode : Ghost_Mode_Type) is
   begin
      Ghost_Mode := Mode;
   end Install_Ghost_Mode;

   procedure Install_Ghost_Mode (Mode : Name_Id) is
   begin
      if Mode = Name_Check then
         Ghost_Mode := Check;

      elsif Mode = Name_Ignore then
         Ghost_Mode := Ignore;

      elsif Mode = Name_None then
         Ghost_Mode := None;
      end if;
   end Install_Ghost_Mode;

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
   begin
      --  Inspect the original node of the unit in case removal of ignored
      --  Ghost code has already taken place.

      return
        Nkind (N) = N_Compilation_Unit
          and then Is_Ignored_Ghost_Entity
                     (Defining_Entity (Original_Node (Unit (N))));
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
      Ignored_Ghost_Units.Release;
      Ignored_Ghost_Units.Locked := True;
   end Lock;

   -----------------------------------
   -- Mark_And_Set_Ghost_Assignment --
   -----------------------------------

   procedure Mark_And_Set_Ghost_Assignment (N : Node_Id) is
      Id : Entity_Id;

   begin
      --  An assignment statement becomes Ghost when its target denotes a Ghost
      --  object. Install the Ghost mode of the target.

      Id := Ghost_Entity (Name (N));

      if Present (Id) then
         if Is_Checked_Ghost_Entity (Id) then
            Install_Ghost_Mode (Check);

         elsif Is_Ignored_Ghost_Entity (Id) then
            Install_Ghost_Mode (Ignore);

            Set_Is_Ignored_Ghost_Node (N);
            Propagate_Ignored_Ghost_Code (N);
         end if;
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

      --  Install the appropriate Ghost mode

      Install_Ghost_Mode (Policy);
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

      --  Install the appropriate Ghost mode

      Install_Ghost_Mode (Policy);
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

      elsif Nkind_In (N, N_Generic_Function_Renaming_Declaration,
                         N_Generic_Package_Declaration,
                         N_Generic_Package_Renaming_Declaration,
                         N_Generic_Procedure_Renaming_Declaration,
                         N_Generic_Subprogram_Declaration,
                         N_Package_Declaration,
                         N_Package_Renaming_Declaration,
                         N_Subprogram_Declaration,
                         N_Subprogram_Renaming_Declaration)
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

      --  Install the appropriate Ghost mode

      Install_Ghost_Mode (Policy);
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

      --  Install the appropriate Ghost mode

      Install_Ghost_Mode (Policy);

      --  Check ghost actuals. Given that this routine is unconditionally
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
            Install_Ghost_Mode (Check);

         elsif Is_Ignored_Ghost_Entity (Id) then
            Install_Ghost_Mode (Ignore);

            Set_Is_Ignored_Ghost_Node (N);
            Propagate_Ignored_Ghost_Code (N);
         end if;
      end if;
   end Mark_And_Set_Ghost_Procedure_Call;

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
         Propagate_Ignored_Ghost_Code (N);
      end if;

      --  Mark all formal parameters when the related node denotes a subprogram
      --  or a body. The traversal is performed via the specification because
      --  the related subprogram or body may be unanalyzed.

      --  ??? could extra formal parameters cause a Ghost leak?

      if Mark_Formals
        and then Nkind_In (N, N_Abstract_Subprogram_Declaration,
                              N_Formal_Abstract_Subprogram_Declaration,
                              N_Formal_Concrete_Subprogram_Declaration,
                              N_Generic_Subprogram_Declaration,
                              N_Subprogram_Body,
                              N_Subprogram_Body_Stub,
                              N_Subprogram_Declaration,
                              N_Subprogram_Renaming_Declaration)
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
   -- Mark_Ghost_Clause --
   -----------------------

   procedure Mark_Ghost_Clause (N : Node_Id) is
      Nam : Node_Id := Empty;

   begin
      if Nkind (N) = N_Use_Package_Clause then
         Nam := First (Names (N));

      elsif Nkind (N) = N_Use_Type_Clause then
         Nam := First (Subtype_Marks (N));

      elsif Nkind (N) = N_With_Clause then
         Nam := Name (N);
      end if;

      if Present (Nam)
        and then Is_Entity_Name (Nam)
        and then Present (Entity (Nam))
        and then Is_Ignored_Ghost_Entity (Entity (Nam))
      then
         Set_Is_Ignored_Ghost_Node (N);
         Propagate_Ignored_Ghost_Code (N);
      end if;
   end Mark_Ghost_Clause;

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
         Propagate_Ignored_Ghost_Code (N);
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

   ----------------------------------
   -- Propagate_Ignored_Ghost_Code --
   ----------------------------------

   procedure Propagate_Ignored_Ghost_Code (N : Node_Id) is
      Nod  : Node_Id;
      Scop : Entity_Id;

   begin
      --  Traverse the parent chain looking for blocks, packages, and
      --  subprograms or their respective bodies.

      Nod := Parent (N);
      while Present (Nod) loop
         Scop := Empty;

         if Nkind (Nod) = N_Block_Statement
           and then Present (Identifier (Nod))
         then
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
            --  Do not prune compilation unit nodes because many mechanisms
            --  depend on their presence. Note that context items are still
            --  being processed.

            if Nkind (N) = N_Compilation_Unit then
               return OK;

            --  The node is either declared as ignored Ghost or is a byproduct
            --  of expansion. Destroy it and stop the traversal on this branch.

            elsif Is_Ignored_Ghost_Node (N) then
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
         Prune_Tree (Ignored_Ghost_Units.Table (Index));
      end loop;
   end Remove_Ignored_Ghost_Code;

   ------------------------
   -- Restore_Ghost_Mode --
   ------------------------

   procedure Restore_Ghost_Mode (Mode : Ghost_Mode_Type) is
   begin
      Ghost_Mode := Mode;
   end Restore_Ghost_Mode;

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

      elsif Nkind_In (N, N_Freeze_Entity, N_Freeze_Generic_Entity) then
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

end Ghost;
