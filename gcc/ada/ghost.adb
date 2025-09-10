------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G H O S T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2014-2025, Free Software Foundation, Inc.         --
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
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Disp;       use Sem_Disp;
with Sem_Eval;       use Sem_Eval;
with Sem_Prag;       use Sem_Prag;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;          use Stand;
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

   ---------------------
   -- Local variables --
   ---------------------

   Assertion_Level_Error_Msg : constant String :=
     "incompatible assertion levels in effect";

   Ghost_Policy_Error_Msg : constant String :=
     "incompatible ghost policies in effect";

   -----------------------
   -- Local subprograms --
   -----------------------

   function Assertion_Level_To_Name (Level : Entity_Id) return Name_Id;
   --  Returns No_Name if there is no Level or the name of the Level.

   procedure Check_Valid_Ghost_Declaration (N : Node_Id);
   --  Check that the declaration for a Ghost node N has a valid
   --  Assertion_Policy and a valid Assertion_Level.

   function Get_Ghost_Aspect (N : Node_Id) return Node_Id;
   --  Returns the Ghost aspect for a given node if it has one.

   function Get_Ghost_Pragma (N : Node_Id) return Node_Id;
   --  Return the Ghost pragma following this node.

   function Get_Ghost_Assertion_Level (N : Node_Id) return Entity_Id;
   --  Returns the Assertion_Level entity if the node has a Ghost aspect and
   --  the Ghost aspect is using an Assertion_Level.

   function Ghost_Assertion_Level_In_Effect (Id : Entity_Id) return Entity_Id;
   --  Returns the ghost level applicable for the given entity Id in a similar
   --  manner as Ghost_Policy_In_Effect.

   function Ghost_Policy_In_Effect (Id : Entity_Id) return Name_Id;
   --  Returns the ghost policy applicable for the given entity Id.
   --
   --  SPARK RM 6.9 (3):
   --
   --  An object declaration which occurs inside an expression in a ghost
   --  declaration, statement, assertion pragma or specification aspect
   --  declaration is a ghost declaration.
   --
   --  If this declaration does not have the Ghost aspect specified, the
   --  assertion policy applicable to this declaration comes from the policy
   --  applicable to the enclosing declaration, statement, assertion pragma
   --  or specification aspect.
   --
   --  If the declaration occurs inside a ghost declaration, ghost statement,
   --  assertion pragma or specification aspect and the assertion policy
   --  applicable to this scope is Ignore, then the assertion policy applicable
   --  to the declaration is also Ignore.
   --
   --  Otherwise, the assertion policy applicable to an object declaration
   --  comes either from its assertion level if any, or from the ghost
   --  policy at the point of declaration.

   procedure Install_Ghost_Region
     (Mode : Name_Id; N : Node_Id; Level : Entity_Id);
   pragma Inline (Install_Ghost_Region);
   --  Install a Ghost region comprised of mode Mode and ignored region start
   --  node N and Level as the Assertion_Level that was associated with it.

   function Is_Subject_To_Ghost (N : Node_Id) return Boolean;
   --  Determine whether declaration or body N is subject to aspect or pragma
   --  Ghost. This routine must be used in cases where pragma Ghost has not
   --  been analyzed yet, but the context needs to establish the "ghostness"
   --  of N.

   procedure Mark_And_Set_Ghost_Region (N : Node_Id; Id : Entity_Id);
   --  Install a new ghost region for N based on the active policy applied for
   --  Id. Additionally if the policy is ignored mark and set the node as an
   --  ignored ghost region.

   procedure Mark_Ghost_Declaration_Or_Body
     (N     : Node_Id;
      Mode  : Name_Id;
      Level : Entity_Id);
   --  Mark the defining entity of declaration or body N as Ghost depending on
   --  mode Mode. Mark all formals parameters when N denotes a subprogram or a
   --  body. Additionally set level as the Ghost_Assertion_Level for all of
   --  them.

   procedure Record_Ignored_Ghost_Node (N : Node_Or_Entity_Id);
   --  Save ignored Ghost node or entity N in table Ignored_Ghost_Nodes for
   --  later elimination.

   ------------------------------
   -- Assertion_Level_From_Arg --
   ------------------------------

   function Assertion_Level_From_Arg (Arg : Node_Id) return Entity_Id is
      Expr  : constant Node_Id := Get_Pragma_Arg (Arg);
      Level : Entity_Id;

   begin
      --  Aspect Ghost without an expression uses Standard_Level_Default

      if No (Expr) then
         return Standard_Level_Default;
      end if;

      --  Check if the expression matches a static boolean expression first

      Preanalyze_And_Resolve_Without_Errors (Expr);
      if Is_OK_Static_Expression (Expr) then
         if Is_True (Expr_Value (Expr)) then
            return Standard_Level_Default;
         else
            --  Ghost => False is considered to be non-ghost

            return Empty;
         end if;
      end if;

      --  Alternatively the argument could be an Assertion_Level

      if Nkind (Expr) = N_Identifier then
         Level := Get_Assertion_Level (Chars (Expr));
         if Present (Level) then
            --  The identifier resolved to an assertion level. Override the
            --  Any_Id from a failed resolution in pre-analysis.

            Set_Entity (Expr, Level);
            return Level;
         end if;
      end if;

      --  We are dealing with a malformed ghost argument.
      --  An error will be emitted when the pragma is analyzed.

      return Empty;
   end Assertion_Level_From_Arg;

   -----------------------------
   -- Assertion_Level_To_Name --
   -----------------------------

   function Assertion_Level_To_Name (Level : Entity_Id) return Name_Id is
   begin
      if No (Level) then
         return No_Name;
      end if;

      return Chars (Level);
   end Assertion_Level_To_Name;

   ----------------------------
   -- Check_Ghost_Completion --
   ----------------------------

   procedure Check_Ghost_Completion
     (Prev_Id  : Entity_Id;
      Compl_Id : Entity_Id)
   is
      Policy : Name_Id;

   begin
      --  Nothing to do if one of the views is missing

      if No (Prev_Id) or else No (Compl_Id) then
         return;
      end if;

      Policy := Ghost_Policy_In_Effect (Prev_Id);

      --  The Ghost policy in effect at the point of declaration and at the
      --  point of completion must match (SPARK RM 6.9(19)).

      if Is_Checked_Ghost_Entity (Prev_Id)
        and then Policy = Name_Ignore
      then
         Error_Msg_N (Ghost_Policy_Error_Msg, Prev_Id);
         Error_Msg_Sloc := Sloc (Prev_Id);
         Error_Msg_N ("\& declared # with ghost policy `Check`", Prev_Id);
         Error_Msg_Sloc := Sloc (Compl_Id);
         Error_Msg_N ("\& completed # with ghost policy `Ignore`", Prev_Id);

      elsif Is_Ignored_Ghost_Entity (Prev_Id)
        and then Policy = Name_Check
      then
         Error_Msg_N (Ghost_Policy_Error_Msg, Prev_Id);
         Error_Msg_Sloc := Sloc (Prev_Id);
         Error_Msg_N ("\& declared # with ghost policy `Ignore`", Prev_Id);
         Error_Msg_Sloc := Sloc (Compl_Id);
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
      --  a Ghost entity can safely reside (SPARK RM 6.9(13)).

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
         --  Determine whether node Prag is a suitable context for a ghost
         --  reference. To qualify as such, Prag must either
         --
         --    * Be an assertion expression pragma
         --
         --    * Denote pragma Global, Depends, Initializes, Refined_Global,
         --      Refined_Depends or Refined_State.
         --
         --    * Specify an aspect of a Ghost entity
         --
         --    * Contain a reference to a Ghost entity

         function Is_OK_Statement
           (Stmt : Node_Id; Id : Entity_Id; Call_Arg : Node_Id) return Boolean;
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

            --  A reference to a Ghost entity may appear within the class-wide
            --  precondition of a helper subprogram. This context is treated
            --  as suitable because it was already verified when we were
            --  analyzing the original class-wide precondition.

            elsif Is_Subprogram (Current_Scope)
              and then Present (Class_Preconditions_Subprogram (Current_Scope))
            then
               return True;

            --  References to Ghost entities may be relocated in internally
            --  generated bodies.

            elsif Nkind (Decl) = N_Subprogram_Body
              and then not Comes_From_Source (Decl)
            then
               Subp_Id := Corresponding_Spec (Decl);

               if Present (Subp_Id) then

                  --  The context is the internally built _Wrapped_Statements
                  --  procedure, which is OK because the real check was done
                  --  before contract expansion activities.

                  if Chars (Subp_Id) = Name_uWrapped_Statements then
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

         function Is_OK_Pragma (Prag : Node_Id) return Boolean
         is
            Prag_Id  : Pragma_Id;
            Prag_Nam : Name_Id;

         begin
            if Nkind (Prag) /= N_Pragma then
               return False;
            end if;

            --  if the entitiy associated with the pragma is ignored then we do
            --  not need to analyze the pragma.

            if Is_Ignored_Ghost_Pragma (Prag) then
               return True;
            end if;

            Prag_Id  := Get_Pragma_Id (Prag);
            Prag_Nam := Original_Aspect_Pragma_Name (Prag);

            --  A pragma may not be analyzed, so that its Ghost status is
            --  not determined yet, but it is guaranteed to be Ghost when
            --  referencing a Ghost entity.

            if Suppressed_Ghost_Policy_Check_Pragma (Prag_Id) then
               return True;

            --  An assertion expression pragma is Ghost when it contains a
            --  reference to a Ghost entity (SPARK RM 6.9(13)), except for
            --  predicate pragmas (SPARK RM 6.9(14)).

            elsif Is_Valid_Assertion_Kind (Prag_Nam)
              and then Assertion_Expression_Pragma (Prag_Id)
              and then Prag_Id /= Pragma_Predicate
            then
               return True;

            --  A pragma that applies to a Ghost construct or specifies an
            --  aspect of a Ghost entity is a Ghost pragma (SPARK RM 6.9(4))

            elsif Is_Ghost_Pragma (Prag) then
               return True;

            --  Several pragmas that may apply to a non-Ghost entity are
            --  treated as Ghost when they contain a reference to a Ghost
            --  entity (SPARK RM 6.9(18)).

            elsif Prag_Nam
                  in Name_Global
                   | Name_Depends
                   | Name_Initializes
                   | Name_Refined_Global
                   | Name_Refined_Depends
                   | Name_Refined_State
            then
               return True;
            end if;

            return False;
         end Is_OK_Pragma;

         ---------------------
         -- Is_OK_Statement --
         ---------------------

         function Is_OK_Statement
           (Stmt : Node_Id; Id : Entity_Id; Call_Arg : Node_Id) return Boolean
         is
            procedure Check_Assignment_Levels (Assignee : Entity_Id);
            --  Check that a ghost entity on the RHS of the assignment is
            --  assertion level dependent on the LHS.

            procedure Check_Procedure_Call_Policies (Callee : Entity_Id);
            --  Check that
            --  * the a checked call argument is not modified by an ignored
            --    procedure call.
            --  * the level of the modified call argument depends on the level
            --    of the call.

            function Is_Modified_By_Call
              (Call : Node_Id; Call_Arg : Node_Id; Callee : Entity_Id)
               return Boolean;
            --  Check that Call_Arg was used in the call and that the formal
            --  for that argument was either out or in-out.

            -----------------------------
            -- Check_Assignment_Levels --
            -----------------------------

            procedure Check_Assignment_Levels (Assignee : Entity_Id) is
               Assignee_Level : constant Entity_Id :=
                 Ghost_Assertion_Level (Assignee);
               Id_Level       : constant Entity_Id :=
                 Ghost_Assertion_Level (Id);
            begin
               --  SPARK RM 6.9 (13) A ghost entity E shall only be referenced
               --  within an assignment statement whose target is a ghost
               --  variable that is assertion-level-dependent on E.

               if not Is_Assertion_Level_Dependent (Assignee_Level, Id_Level)
               then
                  Error_Msg_N (Assertion_Level_Error_Msg, Ghost_Ref);
                  Error_Msg_Name_1 := Chars (Id_Level);
                  Error_Msg_N ("\& has assertion level %", Ghost_Ref);
                  Error_Msg_Name_1 := Chars (Assignee_Level);
                  Error_Msg_Node_2 := Assignee;
                  Error_Msg_NE ("\& is modifying & with %", Ghost_Ref, Id);
                  Error_Msg_Name_1 := Chars (Id_Level);
                  Error_Msg_NE
                    ("\assertion level of & should depend on %",
                     Ghost_Ref,
                     Assignee);
               end if;
            end Check_Assignment_Levels;

            -----------------------------------
            -- Check_Procedure_Call_Policies --
            -----------------------------------

            procedure Check_Procedure_Call_Policies (Callee : Entity_Id) is
               Id_Level  : constant Entity_Id := Ghost_Assertion_Level (Id);
               Id_Policy : constant Name_Id := Ghost_Policy_In_Effect (Id);

               Call_Level  : Entity_Id;
               Call_Policy : Name_Id;
            begin
               if No (Callee) then
                  return;
               end if;

               --  Checks apply only if we are processing a call argument that
               --  is modified by the call.

               if No (Call_Arg)
                 or else not Is_Modified_By_Call (Stmt, Call_Arg, Callee)
               then
                  return;
               end if;

               Call_Policy := Ghost_Policy_In_Effect (Callee);
               Call_Level := Ghost_Assertion_Level (Callee);

               if Id_Policy = Name_Check
                 and then Call_Policy = Name_Ignore
               then
                  Error_Msg_Sloc := Sloc (Ghost_Ref);
                  Error_Msg_N (Ghost_Policy_Error_Msg, Ghost_Ref);
                  Error_Msg_NE ("\& has ghost policy `Check`", Ghost_Ref, Id);
                  Error_Msg_NE
                    ("\& is modified # by a procedure with `Ignore`",
                     Ghost_Ref,
                     Id);
               end if;

               --  An out or in out mode actual parameter and the subprogram
               --  shall have matching assertion levels SPARK RM 6.9 (15).

               if Id_Level /= Call_Level then
                  Error_Msg_N (Assertion_Level_Error_Msg, Ghost_Ref);
                  Error_Msg_Name_1 := Chars (Id_Level);
                  Error_Msg_N ("\& has assertion level %", Ghost_Ref);
                  Error_Msg_Name_1 := Chars (Call_Level);
                  Error_Msg_Node_2 := Callee;
                  Error_Msg_N
                    ("\& is modified by & with %", Ghost_Ref);
                  Error_Msg_N
                    ("\the levels of the call and call arguments must match",
                     Ghost_Ref);
               end if;
            end Check_Procedure_Call_Policies;

            -------------------------
            -- Is_Modified_By_Call --
            -------------------------

            function Is_Modified_By_Call
              (Call : Node_Id; Call_Arg : Node_Id; Callee : Entity_Id)
               return Boolean
            is
               Form : Node_Id;
               Act  : Node_Id;
            begin
               Act := First_Actual (Call);
               Form := First_Formal (Callee);

               while Present (Form) and then Present (Act) loop
                  if Act = Call_Arg then
                     return
                       Ekind (Form) in E_Out_Parameter | E_In_Out_Parameter;
                  end if;

                  Next_Formal (Form);
                  Next_Actual (Act);
               end loop;

               return False;
            end Is_Modified_By_Call;

         --  Start of processing for Is_OK_Statement

         begin
            --  An assignment statement is Ghost when the target is a Ghost
            --  entity.

            if Nkind (Stmt) = N_Assignment_Statement then
               if Is_Ghost_Assignment (Stmt) then
                  Check_Assignment_Levels
                    (Get_Enclosing_Ghost_Entity (Name (Stmt)));
                  return True;
               end if;

            --  A procedure call is Ghost when it calls a Ghost procedure

            elsif Nkind (Stmt) = N_Procedure_Call_Statement then
               if Is_Ghost_Procedure_Call (Stmt) then
                  Check_Procedure_Call_Policies (Get_Subprogram_Entity (Stmt));
                  return True;
               end if;

            --  Special cases

            --  An if statement is a suitable context for a Ghost entity if it
            --  is the byproduct of assertion expression expansion. Note that
            --  the assertion expression may not be related to a Ghost entity,
            --  but it may still contain references to Ghost entities.

            elsif Nkind (Stmt) = N_If_Statement
              and then Comes_From_Check_Or_Contract (Stmt)
            then
               return True;
            end if;

            return False;
         end Is_OK_Statement;

         --  Local variables

         Par  : Node_Id;
         Prev : Node_Id;

      --  Start of processing for Is_OK_Ghost_Context

      begin
         --  Routine Expand_Record_Extension creates a parent subtype without
         --  inserting it into the tree. There is no good way of recognizing
         --  this special case as there is no parent. Try to approximate the
         --  context.

         if No (Parent (Context)) and then Is_Tagged_Type (Ghost_Id) then
            return True;

         --  Otherwise climb the parent chain looking for a suitable Ghost
         --  context.

         else
            Par := Context;
            Prev := Empty;
            while Present (Par) loop
               --  It is not possible to check correct use of Ghost entities
               --  in generic instantiations until after the generic has been
               --  resolved. Postpone that verification to after resolution.

               if Nkind (Par) = N_Generic_Association then
                  return True;

               --  A reference to a Ghost entity can appear within an aspect
               --  specification (SPARK RM 6.9(13)). The precise checking will
               --  occur when analyzing the corresponding pragma. We make an
               --  exception for predicate aspects other than Ghost_Predicate
               --  that only allow referencing a Ghost entity when the
               --  corresponding type declaration is Ghost (SPARK RM 6.9(14)).

               elsif Nkind (Par) = N_Aspect_Specification
                 and then
                   (Get_Aspect_Id (Par) = Aspect_Ghost_Predicate
                     or else not Same_Aspect
                                   (Get_Aspect_Id (Par), Aspect_Predicate))
               then
                  return True;

               --  A Ghost type may be referenced in a use or use_type clause
               --  (SPARK RM 6.9(13)).

               elsif Present (Parent (Par))
                 and then Nkind (Parent (Par)) in N_Use_Package_Clause
                                                | N_Use_Type_Clause
               then
                  return True;

               --  The context is an attribute definition clause for a Ghost
               --  entity.

               elsif Nkind (Parent (Par)) = N_Attribute_Definition_Clause
                 and then Par = Name (Parent (Par))
               then
                  return True;

               --  The context is the instantiation or renaming of a Ghost
               --  entity.

               elsif Nkind (Parent (Par)) in N_Generic_Instantiation
                                           | N_Renaming_Declaration
                                           | N_Generic_Renaming_Declaration
                 and then Par = Name (Parent (Par))
               then
                  return True;

               --  In the case of the renaming of a ghost object, the type
               --  itself may be ghost.

               elsif Nkind (Parent (Par)) = N_Object_Renaming_Declaration
                 and then (Par = Subtype_Mark (Parent (Par))
                             or else Par = Access_Definition (Parent (Par)))
               then
                  return True;

               --  It is always legal to use a ghost prefix. More complex
               --  scenarios are analyzed for the selector.

               elsif Nkind (Par) = N_Selected_Component
                 and then Prefix (Par) = Prev
               then
                  return True;

               elsif Is_OK_Declaration (Par) then
                  return True;

               elsif Is_OK_Pragma (Par) then
                  return True;

               elsif Is_OK_Statement (Par, Ghost_Id, Prev) then
                  return True;

               --  Prevent the search from going too far

               elsif Is_Body_Or_Package_Declaration (Par) then
                  exit;
               end if;

               Prev := Par;
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
         function Is_From_Aspect_Iterable (Ref : Node_Id) return Boolean;
         --  Returns true when the node is contained within an Iterable aspect.

         function Is_From_Aspect_Iterable (Ref : Node_Id) return Boolean is
            P : Node_Id := Parent (Ref);
         begin
            while Present (P) loop
               if Nkind (P) = N_Aspect_Specification then
                  return Get_Aspect_Id (P) = Aspect_Iterable;
               end if;
               P := Parent (P);
            end loop;
            return False;
         end Is_From_Aspect_Iterable;

         --  Local variables

         Applic_Policy : Ghost_Mode_Type := Ghost_Config.Ghost_Mode;
         Ghost_Region  : constant Node_Id := Ghost_Config.Current_Region;

      --  Start of processing for Check_Ghost_Policy

      begin
         --  The policy is allowed to change within renaming and instantiation
         --  statements.

         if No (Ghost_Region)
           or else Nkind (Ghost_Region)
                   in N_Object_Renaming_Declaration
                    | N_Package_Instantiation
                    | N_Procedure_Instantiation
                    | N_Subprogram_Renaming_Declaration
         then
            return;
         end if;

         --  The applied policy for procedure calls is the policy in effect at
         --  the moment of the call.

         if Ekind (Id) in E_Procedure then
            Applic_Policy := Name_To_Ghost_Mode (Ghost_Policy_In_Effect (Id));
         end if;

         --  The Ghost policy in effect a the point of declaration and at the
         --  point of use must match (SPARK RM 6.9(18)).

         if Is_Checked_Ghost_Entity (Id)
           and then Applic_Policy = Ignore
           and then Known_To_Be_Assigned (Ref)
         then
            Error_Msg_N (Ghost_Policy_Error_Msg, Ref);
            Error_Msg_Sloc := Sloc (Id);
            Error_Msg_NE ("\& declared # with ghost policy `Check`", Ref, Id);
            Error_Msg_Sloc := Sloc (Ref);
            Error_Msg_NE ("\& used # with ghost policy `Ignore`", Ref, Id);
         end if;

         --  A ghost entity E shall not be referenced within an aspect
         --  specification [(including an aspect-specifying pragma)] which
         --  specifies an aspect of an entity that is either non-ghost or not
         --  assertion-level-dependent on E except in the following cases the
         --  specified aspect is either Global, Depends, Refined_Global,
         --  Refined_Depends, Initializes, Refined_State, or Iterable (SPARK RM
         --  6.9(14)).

         if No (Ghost_Region)
           or else (Nkind (Ghost_Region) = N_Pragma
                    and then Get_Pragma_Id (Ghost_Region)
                             in Pragma_Global
                              | Pragma_Depends
                              | Pragma_Refined_Global
                              | Pragma_Refined_Depends
                              | Pragma_Initializes
                              | Pragma_Refined_State)
           or else Is_From_Aspect_Iterable (Ref)
         then
            return;
         end if;

         if Is_Ignored_Ghost_Entity (Id) and then Applic_Policy = Check then
            Error_Msg_N (Ghost_Policy_Error_Msg, Ref);
            Error_Msg_Sloc := Sloc (Id);
            Error_Msg_NE ("\& declared # with ghost policy `Ignore`", Ref, Id);
            Error_Msg_Sloc := Sloc (Ref);
            Error_Msg_NE ("\& used # with ghost policy `Check`", Ref, Id);
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
      if Ghost_Context_Checks_Disabled then
         return;
      end if;

      --  Class-wide pre/postconditions of ignored pragmas are preanalyzed
      --  to report errors on wrong conditions; however, ignored pragmas may
      --  also have references to ghost entities and we must disable checking
      --  their context to avoid reporting spurious errors.

      if Inside_Class_Condition_Preanalysis then
         return;
      end if;

      --  When assertions are enabled, compiler generates code for ghost
      --  entities, that is not subject to Ghost policy.

      if not Comes_From_Source (Ghost_Ref) then
         return;
      end if;

      --  If the Ghost entity appears in a non-Ghost context and affects
      --  its behavior or value (SPARK RM 6.9(13,14)).

      if not Is_OK_Ghost_Context (Ghost_Ref) then
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
               & "or use a Ghost_Predicate "
               & "or use a type invariant on a private type", Ghost_Ref);
         end if;
      end if;

      --  Once it has been established that the reference to the Ghost entity
      --  is within a suitable context, ensure that the policy at the point of
      --  declaration and at the point of use match.

      if Present (Ghost_Id) then
         Check_Ghost_Policy (Ghost_Id, Ghost_Ref);
      end if;
   end Check_Ghost_Context;

   ------------------------------------------------
   -- Check_Ghost_Context_In_Generic_Association --
   ------------------------------------------------

   procedure Check_Ghost_Context_In_Generic_Association
     (Actual : Node_Id;
      Formal : Entity_Id)
   is
      function Emit_Error_On_Ghost_Reference
        (N : Node_Id)
         return Traverse_Result;
      --  Determine wether N denotes a reference to a ghost entity, and if so
      --  issue an error.

      -----------------------------------
      -- Emit_Error_On_Ghost_Reference --
      -----------------------------------

      function Emit_Error_On_Ghost_Reference
        (N : Node_Id)
         return Traverse_Result
      is
      begin
         if Is_Entity_Name (N)
           and then Present (Entity (N))
           and then Is_Ghost_Entity (Entity (N))
         then
            Error_Msg_N ("ghost entity cannot appear in this context", N);
            Error_Msg_Sloc := Sloc (Formal);
            Error_Msg_NE ("\formal & was not declared as ghost #", N, Formal);
            return Abandon;
         end if;

         return OK;
      end Emit_Error_On_Ghost_Reference;

      procedure Check_Ghost_References is
        new Traverse_Proc (Emit_Error_On_Ghost_Reference);

   --  Start of processing for Check_Ghost_Context_In_Generic_Association

   begin
      --  The context is ghost when it appears within a Ghost package or
      --  subprogram.

      if Ghost_Config.Ghost_Mode > None then
         return;

      --  The context is ghost if Formal is explicitly marked as ghost

      elsif Is_Ghost_Entity (Formal) then
         return;

      else
         Check_Ghost_References (Actual);
      end if;
   end Check_Ghost_Context_In_Generic_Association;

   -----------------------------------
   -- Check_Valid_Ghost_Declaration --
   -----------------------------------

   procedure Check_Valid_Ghost_Declaration (N : Node_Id) is
      procedure Check_Valid_Assertion_Level (Id : Entity_Id; Ref : Node_Id);
      --  Check that the the assertion level of the declared entity is
      --  compatible with assertion level of the ghost region.

      ---------------------------------
      -- Check_Valid_Assertion_Level --
      ---------------------------------

      procedure Check_Valid_Assertion_Level (Id : Entity_Id; Ref : Node_Id) is
         Id_Level     : constant Entity_Id := Ghost_Assertion_Level (Id);
         Region_Level : constant Entity_Id :=
           Ghost_Config.Ghost_Mode_Assertion_Level;
      begin
         --  This check is not applied for generic isntantiations

         if Is_Generic_Instance (Id) then
            return;
         end if;

         if not Is_Assertion_Level_Dependent (Id_Level, Region_Level) then
            Error_Msg_Sloc := Sloc (Ref);

            Error_Msg_N (Assertion_Level_Error_Msg, Ref);
            Error_Msg_Name_1 := Chars (Id_Level);
            Error_Msg_NE ("\& has assertion level %", Ref, Id);
            Error_Msg_Name_1 := Chars (Region_Level);
            Error_Msg_NE ("\& is declared within a region with %", Ref, Id);
            Error_Msg_Name_1 := Chars (Region_Level);
            Error_Msg_NE ("\assertion level of & should depend on %", Ref, Id);
         end if;
      end Check_Valid_Assertion_Level;

      --  Local variables

      Id : constant Entity_Id := Defining_Entity (N);

   --  Start of processing for Check_Valid_Ghost_Declaration
   begin
      if not Is_Ghost_Entity (Id) or else Ghost_Config.Ghost_Mode = None
      then
         return;
      end if;

      Check_Valid_Assertion_Level (Id, N);
   end Check_Valid_Ghost_Declaration;

   ---------------------------------------------
   -- Check_Ghost_Formal_Procedure_Or_Package --
   ---------------------------------------------

   procedure Check_Ghost_Formal_Procedure_Or_Package
     (N          : Node_Id;
      Actual     : Entity_Id;
      Formal     : Entity_Id;
      Is_Default : Boolean := False)
   is
   begin
      if not Is_Ghost_Entity (Formal) then
         return;
      end if;

      if Present (Actual) and then Is_Ghost_Entity (Actual) then
         return;
      end if;

      if Is_Default then
         Error_Msg_N ("ghost procedure expected as default", N);
         Error_Msg_NE ("\formal & is declared as ghost", N, Formal);

      else
         if Ekind (Formal) = E_Procedure then
            Error_Msg_N ("ghost procedure expected for actual", N);
         else
            Error_Msg_N ("ghost package expected for actual", N);
         end if;

         Error_Msg_Sloc := Sloc (Formal);
         Error_Msg_NE ("\formal & was declared as ghost #", N, Formal);
      end if;
   end Check_Ghost_Formal_Procedure_Or_Package;

   ---------------------------------
   -- Check_Ghost_Formal_Variable --
   ---------------------------------

   procedure Check_Ghost_Formal_Variable
     (Actual     : Node_Id;
      Formal     : Entity_Id;
      Is_Default : Boolean := False)
   is
      Actual_Obj : constant Entity_Id := Get_Enclosing_Ghost_Entity (Actual);
   begin
      if not Is_Ghost_Entity (Formal) then
         return;
      end if;

      if No (Actual_Obj)
        or else not Is_Ghost_Entity (Actual_Obj)
      then
         if Is_Default then
            Error_Msg_N ("ghost object expected as default", Actual);
            Error_Msg_NE ("\formal & is declared as ghost", Actual, Formal);
         else
            Error_Msg_N ("ghost object expected for mutable actual", Actual);
            Error_Msg_Sloc := Sloc (Formal);
            Error_Msg_NE ("\formal & was declared as ghost #", Actual, Formal);
         end if;
      end if;
   end Check_Ghost_Formal_Variable;

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
      if No (Subp) or else No (Overridden_Subp) then
         return;
      end if;

      Over_Subp := Ultimate_Alias (Overridden_Subp);
      Deriv_Typ := Find_Dispatching_Type (Subp);

      --  A Ghost primitive of a non-Ghost type extension cannot override an
      --  inherited non-Ghost primitive (SPARK RM 6.9(10)).

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
      --  inherited Ghost primitive (SPARK RM 6.9(10)).

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
         --  if the primitive is ignored Ghost (SPARK RM 6.9(21)).

         if Is_Ignored_Ghost_Entity (Subp) then

            --  Both the parent subprogram and overriding subprogram are
            --  ignored Ghost.

            if Is_Ignored_Ghost_Entity (Over_Subp) then
               null;

            --  The parent subprogram carries policy Check

            elsif Is_Checked_Ghost_Entity (Over_Subp) then
               Error_Msg_N (Ghost_Policy_Error_Msg, Subp);

               Error_Msg_Sloc := Sloc (Over_Subp);
               Error_Msg_N
                  ("\& declared # with ghost policy `Check`", Subp);

               Error_Msg_Sloc := Sloc (Subp);
               Error_Msg_N
                  ("\overridden # with ghost policy `Ignore`", Subp);

            --  The parent subprogram is non-Ghost

            else
               Error_Msg_N (Ghost_Policy_Error_Msg, Subp);

               Error_Msg_Sloc := Sloc (Over_Subp);
               Error_Msg_N ("\& declared # as non-ghost subprogram", Subp);

               Error_Msg_Sloc := Sloc (Subp);
               Error_Msg_N
                  ("\overridden # with ghost policy `Ignore`", Subp);
            end if;

         --  When a tagged type is either non-Ghost or checked Ghost and
         --  one of its primitives overrides an inherited operation, the
         --  the primitive of the tagged type must be ignored Ghost if the
         --  overridden operation is ignored Ghost (SPARK RM 6.9(21)).

         elsif Is_Ignored_Ghost_Entity (Over_Subp) then

            --  Both the parent subprogram and the overriding subprogram are
            --  ignored Ghost.

            if Is_Ignored_Ghost_Entity (Subp) then
               null;

            --  The overriding subprogram carries policy Check

            elsif Is_Checked_Ghost_Entity (Subp) then
               Error_Msg_N (Ghost_Policy_Error_Msg, Subp);

               Error_Msg_Sloc := Sloc (Over_Subp);
               Error_Msg_N
                  ("\& declared # with ghost policy `Ignore`", Subp);

               Error_Msg_Sloc := Sloc (Subp);
               Error_Msg_N
                  ("\overridden # with Ghost policy `Check`", Subp);

            --  The overriding subprogram is non-Ghost

            else
               Error_Msg_N (Ghost_Policy_Error_Msg, Subp);

               Error_Msg_Sloc := Sloc (Over_Subp);
               Error_Msg_N
                  ("\& declared # with ghost policy `Ignore`", Subp);

               Error_Msg_Sloc := Sloc (Subp);
               Error_Msg_N
                  ("\overridden # with non-ghost subprogram", Subp);
            end if;
         end if;
      end if;
   end Check_Ghost_Overriding;

   ---------------------------
   -- Check_Ghost_Primitive --
   ---------------------------

   procedure Check_Ghost_Primitive (Prim : Entity_Id; Typ : Entity_Id) is
      Prim_Level : Entity_Id;
      Typ_Level : Entity_Id;
   begin
      if not Is_Tagged_Type (Typ) then
         return;
      end if;

      --  The Ghost policy in effect at the point of declaration of a primitive
      --  operation and a tagged type must match (SPARK RM 6.9(21)).

      if Is_Checked_Ghost_Entity (Prim)
        and then Is_Ignored_Ghost_Entity (Typ)
      then
         Error_Msg_N (Ghost_Policy_Error_Msg, Prim);

         Error_Msg_Sloc := Sloc (Typ);
         Error_Msg_NE
            ("\tagged type & declared # with ghost policy `Ignore`",
            Prim, Typ);

         Error_Msg_Sloc := Sloc (Prim);
         Error_Msg_N
            ("\primitive subprogram & declared # with ghost policy `Check`",
            Prim);
      end if;

      Prim_Level := Ghost_Assertion_Level (Prim);
      Typ_Level := Ghost_Assertion_Level (Typ);

      if not Is_Assertion_Level_Dependent (Prim_Level, Typ_Level) then
         Error_Msg_N (Assertion_Level_Error_Msg, Prim);
         Error_Msg_Name_1 := Chars (Typ_Level);
         Error_Msg_Sloc := Sloc (Typ);
         Error_Msg_NE ("\tagged type & declared # with %", Prim, Typ);
         Error_Msg_Name_1 := Chars (Prim_Level);
         Error_Msg_Sloc := Sloc (Prim);
         Error_Msg_NE
           ("\primitive subprogram & declared # with %", Prim, Prim);
         Error_Msg_Name_1 := Chars (Typ_Level);
         Error_Msg_NE
           ("\assertion level of & should depend on %", Prim, Prim);

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
      State_Level   : Entity_Id;
      Constit_Level : Entity_Id;
   begin
      --  Only check ghost states

      if not Is_Ghost_Entity (State_Id) then
         return;
      end if;

      --  A constituent of a Ghost abstract state must be a Ghost entity
      --  (SPARK RM 7.2.2(12)).

      if not Is_Ghost_Entity (Constit_Id) then
         SPARK_Msg_NE
            ("constituent of ghost state & must be ghost",
            Constit, State_Id);
      end if;

      --  The Ghost policy in effect at the point of an ignored abstract state
      --  cannot be check (SPARK RM 6.9(20)).

      if Is_Ignored_Ghost_Entity (State_Id)
        and then Is_Checked_Ghost_Entity (Constit_Id)
      then
         SPARK_Msg_N (Ghost_Policy_Error_Msg, State);
         Error_Msg_Sloc := Sloc (State_Id);
         SPARK_Msg_NE
            ("\abstract state & declared # with ghost policy `Ignore`",
            State, State_Id);
         Error_Msg_Sloc := Sloc (Constit_Id);
         SPARK_Msg_NE
            ("\constituent & declared # with ghost policy `Check`",
            State, Constit_Id);
      end if;

      State_Level := Ghost_Assertion_Level (State_Id);
      Constit_Level := Ghost_Assertion_Level (Constit_Id);

      if not Is_Assertion_Level_Dependent (Constit_Level, State_Level) then
         SPARK_Msg_N (Assertion_Level_Error_Msg, State);
         Error_Msg_Name_1 := Chars (State_Level);
         Error_Msg_Sloc := Sloc (State_Id);
         SPARK_Msg_NE ("\abstract state & declared # with %", State, State_Id);
         Error_Msg_Name_1 := Chars (Constit_Level);
         Error_Msg_Sloc := Sloc (Constit_Id);
         SPARK_Msg_NE ("\constituent & declared # with %", State, Constit_Id);
         Error_Msg_Name_1 := Chars (State_Level);
         SPARK_Msg_NE
           ("\assertion level of & should depend on %", State, Constit_Id);

      end if;
   end Check_Ghost_Refinement;

   ----------------------
   -- Check_Ghost_Type --
   ----------------------

   procedure Check_Ghost_Type (Typ : Entity_Id) is
      Conc_Typ : Entity_Id;
      Full_Typ : Entity_Id;

   begin
      if Is_Ghost_Entity (Typ)
        and then Comes_From_Source (Typ)
      then
         Conc_Typ := Empty;
         Full_Typ := Typ;

         if Is_Single_Concurrent_Type (Typ) then
            Conc_Typ := Anonymous_Object (Typ);
            Full_Typ := Conc_Typ;

         elsif Has_Protected (Typ)
           or else Has_Task (Typ)
         then
            Conc_Typ := Typ;
         end if;

         --  A Ghost type cannot be concurrent (SPARK RM 6.9(22)). Verify this
         --  legality rule first to give a finer-grained diagnostic.

         if Present (Conc_Typ) then
            Error_Msg_N ("ghost type & cannot be concurrent", Conc_Typ);
         end if;

         --  A Ghost type cannot be effectively volatile (SPARK RM 6.9(9))

         if Is_Effectively_Volatile (Full_Typ) then
            Error_Msg_N ("ghost type & cannot be volatile", Full_Typ);
         end if;
      end if;
   end Check_Ghost_Type;

   ----------------------
   -- Get_Ghost_Aspect --
   ----------------------

   function Get_Ghost_Aspect (N : Node_Id) return Node_Id is
      Asp : Node_Id;
   begin
      if not Permits_Aspect_Specifications (N) then
         return Empty;
      end if;

      Asp := First (Aspect_Specifications (N));
      while Present (Asp) loop
         if Chars (Identifier (Asp)) = Name_Ghost then
            return Asp;
         end if;

         Next (Asp);
      end loop;

      return Empty;
   end Get_Ghost_Aspect;

   ----------------------
   -- Get_Ghost_Pragma --
   ----------------------

   function Get_Ghost_Pragma (N : Node_Id) return Node_Id is
      Decl : Node_Id := Empty;
   begin
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
            return Decl;

         --  A source construct ends the region where pragma Ghost may appear,
         --  stop the traversal. Check the original node as source constructs
         --  may be rewritten into something else by expansion.

         elsif Comes_From_Source (Original_Node (Decl)) then
            exit;
         end if;

         Next (Decl);
      end loop;

      return Empty;
   end Get_Ghost_Pragma;

   -------------------------------
   -- Get_Ghost_Assertion_Level --
   -------------------------------

   function Get_Ghost_Assertion_Level (N : Node_Id) return Entity_Id is
      Ghost_Asp  : constant Node_Id := Get_Ghost_Aspect (N);
      Ghost_Prag : Node_Id;

   begin
      if Present (Ghost_Asp) then
         return Assertion_Level_From_Arg (Expression (Ghost_Asp));
      end if;

      Ghost_Prag := Get_Ghost_Pragma (N);
      if Present (Ghost_Prag) then
         return
           Assertion_Level_From_Arg
             (First (Pragma_Argument_Associations (Ghost_Prag)));
      end if;

      return Empty;
   end Get_Ghost_Assertion_Level;

   -------------------------------------
   -- Ghost_Assertion_Level_In_Effect --
   -------------------------------------

   function Ghost_Assertion_Level_In_Effect (Id : Entity_Id) return Entity_Id
   is
   begin
      if Ghost_Config.Is_Inside_Statement_Or_Pragma
        and then Is_Implicit_Ghost (Id)
      then
         return Ghost_Config.Ghost_Mode_Assertion_Level;
      else
         return Ghost_Assertion_Level (Id);
      end if;
   end Ghost_Assertion_Level_In_Effect;

   ----------------------------
   -- Ghost_Policy_In_Effect --
   ----------------------------

   function Ghost_Policy_In_Effect (Id : Entity_Id) return Name_Id is
      Level     : constant Entity_Id := Ghost_Assertion_Level (Id);
      Level_Nam : constant Name_Id :=
        (if No (Level) then No_Name else Chars (Level));
   begin
      if Present (Ghost_Config.Ignored_Ghost_Region) then
         return Name_Ignore;
      elsif Ghost_Config.Is_Inside_Statement_Or_Pragma
        and then Is_Implicit_Ghost (Id)
      then
         case Ghost_Config.Ghost_Mode is
            when Check =>
               return Name_Check;

            when Ignore =>
               return Name_Ignore;

            when None =>
               return No_Name;
         end case;
      else
         return Policy_In_Effect (Name_Ghost, Level_Nam);
      end if;
   end Ghost_Policy_In_Effect;

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

   --------------------------
   -- Install_Ghost_Region --
   --------------------------

   procedure Install_Ghost_Region
     (Mode  : Ghost_Mode_Type;
      N     : Node_Id;
      Level : Entity_Id)
   is
   begin
      --  The context is already within an ignored Ghost region. Maintain the
      --  start of the outermost ignored Ghost region.

      if Present (Ghost_Config.Ignored_Ghost_Region) then
         null;

      --  The current region is the outermost ignored Ghost region. Save its
      --  starting node.

      elsif Present (N) and then Mode = Ignore then
         Ghost_Config.Ignored_Ghost_Region := N;

      --  Otherwise the current region is not ignored, nothing to save

      else
         Ghost_Config.Ignored_Ghost_Region := Empty;
      end if;

      Ghost_Config.Current_Region := N;
      Ghost_Config.Ghost_Mode := Mode;
      Ghost_Config.Ghost_Mode_Assertion_Level := Level;

      if Nkind (Ghost_Config.Current_Region)
         in N_Statement_Other_Than_Procedure_Call
          | N_Procedure_Call_Statement
          | N_Pragma
      then
         Ghost_Config.Is_Inside_Statement_Or_Pragma := True;
      end if;
   end Install_Ghost_Region;

   procedure Install_Ghost_Region
     (Mode : Name_Id; N : Node_Id; Level : Entity_Id) is
   begin
      Install_Ghost_Region (Name_To_Ghost_Mode (Mode), N, Level);
   end Install_Ghost_Region;

   -------------------------
   -- Is_Compatible_Level --
   -------------------------

   function Is_Assertion_Level_Dependent
     (Self : Entity_Id; Other : Entity_Id) return Boolean is
   begin
      return
        Self = Standard_Level_Default
        or else Other = Standard_Level_Default
        or else Is_Same_Or_Depends_On_Level (Self, Other)
        or else Is_Same_Or_Depends_On_Level (Self, Standard_Level_Static);
   end Is_Assertion_Level_Dependent;

   -------------------------
   -- Is_Ghost_Assignment --
   -------------------------

   function Is_Ghost_Assignment (N : Node_Id) return Boolean is
      Id : Entity_Id;

   begin
      --  An assignment statement is Ghost when its target denotes a Ghost
      --  entity.

      if Nkind (N) = N_Assignment_Statement then
         Id := Get_Enclosing_Ghost_Entity (Name (N));

         return Present (Id) and then Is_Ghost_Entity (Id);
      end if;

      return False;
   end Is_Ghost_Assignment;

   ----------------------------------
   -- Is_Ghost_Attribute_Reference --
   ----------------------------------

   function Is_Ghost_Attribute_Reference (N : Node_Id) return Boolean is
   begin
      return Nkind (N) = N_Attribute_Reference
        and then Attribute_Name (N) = Name_Initialized;
   end Is_Ghost_Attribute_Reference;

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
         Id := Get_Enclosing_Ghost_Entity (Name (N));

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
         while Is_Rewrite_Substitution (Res) loop
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
          and then Is_Ignored_Ghost_Entity_In_Codegen
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
      begin
         --  Ghostness is enabled if the argument implies a default assertion
         --  level or it is explicitly a reference to an assertion level.

         return Present (Assertion_Level_From_Arg (Arg));
      end Enables_Ghostness;

      --  Local variables

      Id      : constant Entity_Id := Defining_Entity (N);
      Asp     : Node_Id;
      Prag    : Node_Id;
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

      Asp := Get_Ghost_Aspect (N);

      if Present (Asp) then
         return Enables_Ghostness (Expression (Asp));
      end if;

      --  Examine the following pragmas for an applicable Ghost pragma

      Prag := Get_Ghost_Pragma (N);

      if Present (Prag) then
         return
           Enables_Ghostness (First (Pragma_Argument_Associations (Prag)));
      end if;

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

      procedure Check_Assignment_Policies (Assignee : Entity_Id);
      --  Check that:
      --  * A checked ghost assignment is not used in an ignored ghost
      --    region.
      --  * The level of the ghost region depends on the level of the
      --    ghost assignment.

      -------------------------------
      -- Check_Assignment_Policies --
      -------------------------------

      procedure Check_Assignment_Policies (Assignee : Entity_Id) is
         Assignee_Policy : constant Name_Id :=
           Ghost_Policy_In_Effect (Assignee);
         Assignee_Level  : constant Entity_Id :=
           Ghost_Assertion_Level (Assignee);
         Region_Policy   : constant Ghost_Mode_Type := Ghost_Config.Ghost_Mode;
         Region_Level    : constant Entity_Id :=
           Ghost_Config.Ghost_Mode_Assertion_Level;
      begin
         if Assignee_Policy = Name_Check and then Region_Policy = Ignore then
            Error_Msg_N (Ghost_Policy_Error_Msg, N);
            Error_Msg_NE ("\& has ghost policy `Check`", N, Assignee);
            Error_Msg_NE
              ("\& is modified in a region with `Ignore`", N, Assignee);
         end if;

         --  If an assignment to a part of a ghost variable occurs in a ghost
         --  entity, then the variable should be assertion-level-dependent on
         --  this entity (SPARK RM 6.9(18)).

         if Present (Region_Level)
           and then not Is_Assertion_Level_Dependent
                          (Assignee_Level, Region_Level)
         then
            Error_Msg_N (Assertion_Level_Error_Msg, N);
            Error_Msg_Name_1 := Chars (Assignee_Level);
            Error_Msg_NE ("\& has assertion level %", N, Assignee);
            Error_Msg_Name_1 := Chars (Region_Level);
            Error_Msg_NE
              ("\& is modified within a region with %", N, Assignee);
            Error_Msg_Name_1 := Chars (Region_Level);
            Error_Msg_NE
              ("\assertion level of & should depend on %", N, Assignee);
         end if;
      end Check_Assignment_Policies;

      --  Local variables

      Orig_Lhs : constant Node_Id := Name (N);
      Id       : Entity_Id;
      Lhs      : Node_Id;

   --  Start of processing for Mark_And_Set_Ghost_Assignment

   begin
      --  Cases where full analysis is needed, involving array indexing
      --  which would otherwise be missing array-bounds checks:

      if not Analyzed (Orig_Lhs)
         and then
             ((Nkind (Orig_Lhs) = N_Indexed_Component
                 and then Nkind (Prefix (Orig_Lhs)) = N_Selected_Component
                 and then Nkind (Prefix (Prefix (Orig_Lhs))) =
                          N_Indexed_Component)
            or else
              (Nkind (Orig_Lhs) = N_Selected_Component
                 and then Nkind (Prefix (Orig_Lhs)) = N_Indexed_Component
                 and then Nkind (Prefix (Prefix (Orig_Lhs))) =
                          N_Selected_Component
                 and then Nkind (Parent (N)) /= N_Loop_Statement))
      then
         Analyze (Orig_Lhs);
      end if;

      --  Make sure Lhs is at least preanalyzed, so we can tell whether
      --  it denotes a ghost variable. In some cases we need to do a full
      --  analysis, or else the back end gets confused. Note that in the
      --  preanalysis case, we are preanalyzing a copy of the left-hand
      --  side name, temporarily attached to the tree.

      Lhs :=
        (if Analyzed (Orig_Lhs) then Orig_Lhs else New_Copy_Tree (Orig_Lhs));
      if not Analyzed (Lhs) then
         Set_Name   (N, Lhs);
         Set_Parent (Lhs, N);
         Preanalyze_Without_Errors (Lhs);
         Set_Name (N, Orig_Lhs);
      end if;

      Id := Get_Enclosing_Ghost_Entity (Lhs);

      if Present (Id) and then Is_Ghost_Entity (Id) then
         Check_Assignment_Policies (Id);
      end if;

      Mark_And_Set_Ghost_Region (N, Id);
   end Mark_And_Set_Ghost_Assignment;

   -----------------------------
   -- Mark_And_Set_Ghost_Body --
   -----------------------------

   procedure Mark_And_Set_Ghost_Body (N : Node_Id; Spec_Id : Entity_Id) is
      Body_Id : constant Entity_Id := Defining_Entity (N);
      Level   : Entity_Id := Empty;
      Policy  : Name_Id := No_Name;

   begin
      --  A body becomes Ghost when it is subject to aspect or pragma Ghost

      if Is_Subject_To_Ghost (N) then
         if Present (Spec_Id) then
            Policy := Ghost_Policy_In_Effect (Spec_Id);
            Level := Ghost_Assertion_Level_In_Effect (Spec_Id);
         else
            Policy := Ghost_Policy_In_Effect (Body_Id);
            Level := Ghost_Assertion_Level_In_Effect (Body_Id);
         end if;

      --  A body declared within a Ghost region is automatically Ghost
      --  (SPARK RM 6.9(2)).

      elsif Ghost_Config.Ghost_Mode = Check then
         Policy := Name_Check;
         Level := Ghost_Config.Ghost_Mode_Assertion_Level;

      elsif Ghost_Config.Ghost_Mode = Ignore then
         Policy := Name_Ignore;
         Level := Ghost_Config.Ghost_Mode_Assertion_Level;

      --  Inherit the "ghostness" of the previous declaration when the body
      --  acts as a completion.

      elsif Present (Spec_Id) then
         if Is_Checked_Ghost_Entity (Spec_Id) then
            Policy := Name_Check;

         elsif Is_Ignored_Ghost_Entity (Spec_Id) then
            Policy := Name_Ignore;
         end if;

         Level := Ghost_Assertion_Level (Spec_Id);
      end if;

      --  The Ghost policy in effect at the point of declaration and at the
      --  point of completion must match (SPARK RM 6.9(18)).

      Check_Ghost_Completion (Prev_Id => Spec_Id, Compl_Id => Body_Id);

      --  Mark the body as its formals as Ghost

      Mark_Ghost_Declaration_Or_Body (N, Policy, Level);

      --  Install the appropriate Ghost region

      Install_Ghost_Region (Policy, N, Level);
   end Mark_And_Set_Ghost_Body;

   -----------------------------------
   -- Mark_And_Set_Ghost_Completion --
   -----------------------------------

   procedure Mark_And_Set_Ghost_Completion
     (N       : Node_Id;
      Prev_Id : Entity_Id)
   is
      Compl_Id : constant Entity_Id := Defining_Entity (N);
      Level    : Entity_Id := Empty;
      Policy   : Name_Id := No_Name;

   begin
      --  A completion elaborated in a Ghost region is automatically Ghost
      --  (SPARK RM 6.9(2)).

      if Ghost_Config.Ghost_Mode = Check then
         Policy := Name_Check;
         Level  := Ghost_Config.Ghost_Mode_Assertion_Level;

      elsif Ghost_Config.Ghost_Mode = Ignore then
         Policy := Name_Ignore;
         Level  := Ghost_Config.Ghost_Mode_Assertion_Level;

      --  The completion becomes Ghost when its initial declaration is also
      --  Ghost.

      elsif Is_Checked_Ghost_Entity (Prev_Id) then
         Policy := Name_Check;
         Level  := Ghost_Assertion_Level (Prev_Id);

      elsif Is_Ignored_Ghost_Entity (Prev_Id) then
         Policy := Name_Ignore;
         Level  := Ghost_Assertion_Level (Prev_Id);
      end if;

      --  The Ghost policy in effect at the point of declaration and at the
      --  point of completion must match (SPARK RM 6.9(18)).

      Check_Ghost_Completion
        (Prev_Id  => Prev_Id,
         Compl_Id => Compl_Id);

      --  Mark the completion as Ghost

      Mark_Ghost_Declaration_Or_Body (N, Policy, Level);

      --  Install the appropriate Ghost region

      Install_Ghost_Region (Policy, N, Level);
   end Mark_And_Set_Ghost_Completion;

   ------------------------------------
   -- Mark_And_Set_Ghost_Declaration --
   ------------------------------------

   procedure Mark_And_Set_Ghost_Declaration (N : Node_Id) is
      Level  : Entity_Id := Empty;
      Par_Id : Entity_Id;
      Policy : Name_Id := No_Name;

   begin
      --  A declaration becomes Ghost when it is subject to aspect or pragma
      --  Ghost.

      Level := Get_Ghost_Assertion_Level (N);

      --  A valid assertion level from an explicit pragma or aspect ghost
      --  indicates the explicit ghostlyness of the declaration. Otherwise the
      --  ghostliness of the declaration should be handled by other means like
      --  the region.

      if Present (Level) then
         --  Default to the Ignore policy inside ignored ghost regions.
         --  Similarly to how we do it in Ghost_Policy_In_Effect.
         --  SPARK RM 6.9 (3)

         if Present (Ghost_Config.Ignored_Ghost_Region) then
            Policy := Name_Ignore;
         else
            Policy :=
              Policy_In_Effect (Name_Ghost, Assertion_Level_To_Name (Level));
         end if;

      --  A declaration elaborated in a Ghost region is automatically Ghost
      --  (SPARK RM 6.9(2)).

      elsif Ghost_Config.Ghost_Mode = Check then
         Policy := Name_Check;
         Level := Ghost_Config.Ghost_Mode_Assertion_Level;

      elsif Ghost_Config.Ghost_Mode = Ignore then
         Policy := Name_Ignore;
         Level := Ghost_Config.Ghost_Mode_Assertion_Level;

      --  A child package or subprogram declaration becomes Ghost when its
      --  parent is Ghost (SPARK RM 6.9(2)).

      elsif Nkind (N)
            in N_Generic_Function_Renaming_Declaration
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

         Level := Ghost_Assertion_Level (Par_Id);
      end if;

      --  Mark the declaration and its formals as Ghost

      Mark_Ghost_Declaration_Or_Body (N, Policy, Level);

      Check_Valid_Ghost_Declaration (N);

      --  Install the appropriate Ghost region

      Install_Ghost_Region (Policy, N, Level);
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

      Level  : Entity_Id := Empty;
      Policy : Name_Id := No_Name;

   begin
      --  An instantiation becomes Ghost when it is subject to pragma Ghost

      if Is_Subject_To_Ghost (N) then
         Policy := Ghost_Policy_In_Effect (Gen_Id);

      --  An instantiation declaration within a Ghost region is automatically
      --  Ghost (SPARK RM 6.9(2)).

      elsif Ghost_Config.Ghost_Mode = Check then
         Policy := Name_Check;
         Level  := Ghost_Config.Ghost_Mode_Assertion_Level;

      elsif Ghost_Config.Ghost_Mode = Ignore then
         Policy := Name_Ignore;
         Level  := Ghost_Config.Ghost_Mode_Assertion_Level;

      --  Inherit the "ghostness" of the generic unit, but the current Ghost
      --  policy is the relevant one for the instantiation.

      elsif Is_Ghost_Entity (Gen_Id) then
         Policy := Ghost_Policy_In_Effect (Gen_Id);

         if Policy = No_Name then
            Policy := Name_Ignore;
         end if;

         Level := Ghost_Assertion_Level (Gen_Id);
      end if;

      --  Mark the instantiation as Ghost

      Mark_Ghost_Declaration_Or_Body (N, Policy, Level);

      --  Install the appropriate Ghost region

      Install_Ghost_Region (Policy, N, Level);

      --  Check Ghost actuals. Given that this routine is unconditionally
      --  invoked with subprogram and package instantiations, this check
      --  verifies the context of all the ghost entities passed in generic
      --  instantiations.

      Check_Ghost_Actuals;
   end Mark_And_Set_Ghost_Instantiation;

   ------------------------------------------
   -- Check_Procedure_Call_Argument_Levels --
   ------------------------------------------

   procedure Check_Procedure_Call_Argument_Levels (N : Node_Id) is
      procedure Check_Argument_Levels
        (Actual : Entity_Id; Actual_Ref : Node_Id);
      --  Check that the ghost assertion level of an actual is an assertion
      --  level which depends on the ghost region where the procedure call
      --  is located.

      ---------------------------
      -- Check_Argument_Levels --
      ---------------------------

      procedure Check_Argument_Levels
        (Actual : Entity_Id; Actual_Ref : Node_Id)
      is
         Actual_Level : constant Entity_Id := Ghost_Assertion_Level (Actual);
         Region_Level : constant Entity_Id :=
           Ghost_Config.Ghost_Mode_Assertion_Level;
      begin
         --  If an assignment to a part of a ghost variable occurs in a ghost
         --  entity, then the variable should be assertion-level-dependent on
         --  this entity [This includes both assignment statements and passing
         --  a ghost variable as an out or in out mode actual parameter.]
         --  (SPARK RM 6.9(18)).

         if Present (Region_Level)
           and then not Is_Assertion_Level_Dependent
                          (Actual_Level, Region_Level)
         then
            Error_Msg_N (Assertion_Level_Error_Msg, Actual_Ref);
            Error_Msg_Name_1 := Chars (Actual_Level);
            Error_Msg_N ("\& has assertion level %", Actual_Ref);
            Error_Msg_Name_1 := Chars (Region_Level);
            Error_Msg_N ("\& is modified within a region with %", Actual_Ref);
            Error_Msg_Name_1 := Chars (Region_Level);
            Error_Msg_N
              ("\assertion level of & should depend on %", Actual_Ref);
         end if;
      end Check_Argument_Levels;

      --  Local variables

      Actual      : Node_Id;
      Actual_Id   : Entity_Id;
      Formal      : Node_Id;
      Id          : Entity_Id;
      Orig_Actual : Node_Id;

   --  Start of processing for Check_Procedure_Call_Argument_Levels

   begin
      if Nkind (N) not in N_Procedure_Call_Statement then
         return;
      end if;

      --  Handle the access-to-subprogram case

      if Ekind (Etype (Name (N))) = E_Subprogram_Type then
         Id := Etype (Name (N));
      else
         Id := Get_Enclosing_Ghost_Entity (Name (N));
      end if;

      --  Check for context if we are able to derive the called subprogram and
      --  we are not dealing with an expanded construct.

      if Present (Id)
        and then Comes_From_Source (N)
        and then Ghost_Config.Ghost_Mode /= None
      then
         Orig_Actual := First_Actual (N);
         Formal := First_Formal (Id);

         while Present (Orig_Actual) loop
            --  Similarly to Mark_And_Set_Ghost_Procedure_Call we need to
            --  analyze the call argument first to get its level for this
            --  analysis.

            Actual :=
              (if Analyzed (Orig_Actual)
               then Orig_Actual
               else New_Copy_Tree (Orig_Actual));
            if not Analyzed (Actual) then
               Preanalyze_Without_Errors (Actual);
            end if;

            if Ekind (Formal) in E_Out_Parameter | E_In_Out_Parameter then
               Actual_Id := Get_Enclosing_Ghost_Entity (Actual);
               if Present (Actual_Id) then
                  Check_Argument_Levels (Actual_Id, Orig_Actual);
               end if;
            end if;

            Next_Formal (Formal);
            Next_Actual (Orig_Actual);
         end loop;
      end if;
   end Check_Procedure_Call_Argument_Levels;

   ---------------------------------------
   -- Mark_And_Set_Ghost_Procedure_Call --
   ---------------------------------------

   procedure Mark_And_Set_Ghost_Procedure_Call (N : Node_Id) is
      Id : Entity_Id;
   begin
      --  A procedure call becomes Ghost when the procedure being invoked is
      --  Ghost. Install the Ghost mode of the procedure.

      Id := Get_Enclosing_Ghost_Entity (Name (N));

      Mark_And_Set_Ghost_Region (N, Id);
   end Mark_And_Set_Ghost_Procedure_Call;

   -------------------------------
   -- Mark_And_Set_Ghost_Region --
   -------------------------------

   procedure Mark_And_Set_Ghost_Region (N : Node_Id; Id : Entity_Id) is
      Id_Policy : Name_Id;
   begin
      --  Nothing to do if we are not dealing with a ghost entity

      if No (Id) or else not Is_Ghost_Entity (Id) then
         return;
      end if;

      Id_Policy := Ghost_Policy_In_Effect (Id);

      if Id_Policy = Name_Check then
         Install_Ghost_Region (Check, N, Ghost_Assertion_Level (Id));

      elsif Id_Policy = Name_Ignore then
         Install_Ghost_Region (Ignore, N, Ghost_Assertion_Level (Id));
         Set_Is_Ignored_Ghost_Node (N);
         Record_Ignored_Ghost_Node (N);
      end if;
   end Mark_And_Set_Ghost_Region;

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
        and then Is_Ignored_Ghost_Entity_In_Codegen (Entity (Nam))
      then
         Set_Is_Ignored_Ghost_Node (N);
         Record_Ignored_Ghost_Node (N);
      end if;
   end Mark_Ghost_Clause;

   ------------------------------------
   -- Mark_Ghost_Declaration_Or_Body --
   ------------------------------------

   procedure Mark_Ghost_Declaration_Or_Body
     (N     : Node_Id;
      Mode  : Name_Id;
      Level : Entity_Id)
   is
      Id : constant Entity_Id := Defining_Entity (N);

      Mark_Formals : Boolean := False;
      Param        : Node_Id;
      Param_Id     : Entity_Id;

      procedure Mark_And_Set_Is_Checked_Ghost_Entity (E : Entity_Id);
      --  Sets Is_Checked_Ghost_Entity, unsets Is_Ignored_Ghost_Entity

      procedure Mark_And_Set_Is_Ignored_Ghost_Entity (E : Entity_Id);
      --  Sets Is_Ignored_Ghost_Entity, unsets Is_Checked_Ghost_Entity

      ------------------------------------------
      -- Mark_And_Set_Is_Checked_Ghost_Entity --
      ------------------------------------------

      procedure Mark_And_Set_Is_Checked_Ghost_Entity (E : Entity_Id) is
      begin
         Set_Is_Checked_Ghost_Entity (E, True);
         Set_Is_Ignored_Ghost_Entity (E, False);
      end Mark_And_Set_Is_Checked_Ghost_Entity;

      ------------------------------------------
      -- Mark_And_Set_Is_Ignored_Ghost_Entity --
      ------------------------------------------

      procedure Mark_And_Set_Is_Ignored_Ghost_Entity (E : Entity_Id) is
      begin
         Set_Is_Checked_Ghost_Entity (E, False);
         Set_Is_Ignored_Ghost_Entity (E, True);
      end Mark_And_Set_Is_Ignored_Ghost_Entity;

   --  Start of processing for Mark_Ghost_Declaration_Or_Body

   begin
      Set_Ghost_Assertion_Level (Id, Level);

      if Mode = Name_Check then
         Mark_Formals := True;
         Mark_And_Set_Is_Checked_Ghost_Entity (Id);

      elsif Mode = Name_Ignore then
         Mark_Formals := True;
         Mark_And_Set_Is_Ignored_Ghost_Entity (Id);

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

            Set_Ghost_Assertion_Level (Param_Id, Level);

            if Mode = Name_Check then
               Mark_And_Set_Is_Checked_Ghost_Entity (Param_Id);

            elsif Mode = Name_Ignore then
               Mark_And_Set_Is_Ignored_Ghost_Entity (Param_Id);
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
         Mark_Ghost_Pragma (N, Check);

      elsif Is_Ignored_Ghost_Entity (Id) then
         Mark_Ghost_Pragma (N, Ignore);
      end if;
   end Mark_Ghost_Pragma;

   procedure Mark_Ghost_Pragma (N : Node_Id; Mode : Ghost_Mode_Type) is
   begin
      if Mode = Check then
         Set_Is_Checked_Ghost_Pragma (N, True);
         Set_Is_Ignored_Ghost_Pragma (N, False);

      else
         Set_Is_Checked_Ghost_Pragma (N, False);
         Set_Is_Ignored_Ghost_Pragma (N, True);
         Set_Is_Ignored_Ghost_Node (N);
         Record_Ignored_Ghost_Node (N);
      end if;
   end Mark_Ghost_Pragma;

   -------------------------
   -- Mark_Ghost_Renaming --
   -------------------------

   procedure Mark_Ghost_Renaming (N : Node_Id; Id : Entity_Id) is
      Policy : Name_Id := No_Name;
      Level  : constant Entity_Id := Ghost_Assertion_Level (Id);
   begin
      --  A renaming becomes Ghost when it renames a Ghost entity

      if Is_Checked_Ghost_Entity (Id) then
         Policy := Name_Check;

      elsif Is_Ignored_Ghost_Entity (Id) then
         Policy := Name_Ignore;
      end if;

      Mark_Ghost_Declaration_Or_Body (N, Policy, Level);
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

   procedure Restore_Ghost_Region (Config : Ghost_Config_Type) is
   begin
      Ghost_Config := Config;
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
         Level : constant Entity_Id := Ghost_Assertion_Level (Id);
      begin
         if Is_Checked_Ghost_Entity (Id) then
            Install_Ghost_Region (Check, N, Level);
         elsif Is_Ignored_Ghost_Entity (Id) then
            Install_Ghost_Region (Ignore, N, Level);
         else
            Install_Ghost_Region (None, N, Level);
         end if;
      end Set_Ghost_Mode_From_Entity;

      --  Local variables

      Id    : Entity_Id;
      Level : Entity_Id;

   --  Start of processing for Set_Ghost_Mode

   begin
      --  The Ghost mode of an assignment statement depends on the Ghost mode
      --  of the target.

      if Nkind (N) = N_Assignment_Statement then
         Id := Get_Enclosing_Ghost_Entity (Name (N));

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
         Level := Pragma_Ghost_Assertion_Level (N);

         if Is_Checked_Ghost_Pragma (N) then

            --  Still install an ignored ghost region if the pragma is attached
            --  to a checked ghost entity, but the pragma itself is explicitly
            --  ignored.

            if Is_Ignored (N) then
               Install_Ghost_Region (Ignore, N, Level);
            else
               Install_Ghost_Region (Check, N, Level);
            end if;
         elsif Is_Ignored_Ghost_Pragma (N) then
            Install_Ghost_Region (Ignore, N, Level);
         else
            if Is_Checked (N) then
               Install_Ghost_Region (Check, N, Level);
            else
               Install_Ghost_Region (None, N, Level);
            end if;
         end if;

      --  The Ghost mode of a procedure call depends on the Ghost mode of the
      --  procedure being invoked.

      elsif Nkind (N) = N_Procedure_Call_Statement then
         Id := Get_Enclosing_Ghost_Entity (Name (N));

         if Present (Id) then
            Set_Ghost_Mode_From_Entity (Id);
         end if;
      end if;
   end Set_Ghost_Mode;

   -------------------------
   -- Set_Is_Ghost_Entity --
   -------------------------

   procedure Set_Is_Ghost_Entity (Id : Entity_Id) is
      Policy : constant Name_Id := Ghost_Policy_In_Effect (Id);
   begin
      if Policy = Name_Check then
         Set_Is_Checked_Ghost_Entity (Id);
      elsif Policy = Name_Ignore then
         Set_Is_Ignored_Ghost_Entity (Id);
      end if;
   end Set_Is_Ghost_Entity;

end Ghost;
