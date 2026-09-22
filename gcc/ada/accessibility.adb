------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        A C C E S S I B I L I T Y                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2022-2026, Free Software Foundation, Inc.         --
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

with Atree;          use Atree;
with Checks;         use Checks;
with Debug;          use Debug;
with Einfo.Entities; use Einfo.Entities;
with Elists;         use Elists;
with Errout;         use Errout;
with Einfo.Utils;    use Einfo.Utils;
with Exp_Atag;       use Exp_Atag;
with Exp_Ch7;        use Exp_Ch7;
with Exp_Tss;        use Exp_Tss;
with Exp_Util;       use Exp_Util;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Restrict;       use Restrict;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Eval;       use Sem_Eval;
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;          use Stand;
with Tbuild;         use Tbuild;

package body Accessibility is

   -----------------------
   -- Local Subprograms --
   -----------------------

   type Accessibility_Level_Kind is
     (Dynamic_Level, Object_Decl_Level, Library_On_Dynamic_Level);
   --  Captures the different modes in which an accessibility level could be
   --  obtained for a given expression. In the context of Accessibility_Level
   --  function, Accessibility_Level_Kind signals what type of accessibility
   --  level to obtain.

   --  When Level is Dynamic_Level, a defining identifier associated with an
   --  access parameter, an access result, or an SAOOAAAT, may be returned.
   --  For a conditional expression, another conditional expression may be
   --  returned. An N_Integer_Literal node is returned in the other cases.

   --  When Level is Object_Decl_Level, an N_Integer_Literal node whose value
   --  is the level of the declaration of the object is returned in the cases
   --  where Dynamic_Level returns a defining identifier. For a conditional
   --  expression whose dependent expression is *not* statically selected, an
   --  assertion failure is raised. In the other cases, an N_Integer_Literal
   --  node is returned like for Dynamic_Level.

   --  When Level is Library_On_Dynamic_Level, an N_Integer_Literal node whose
   --  value is the library level is returned in the cases where Dynamic_Level
   --  returns a defining identifier. Likewise for a conditional expression
   --  whose dependent expression is *not* statically selected. In the other
   --  cases, an N_Integer_Literal node is returned like for Dynamic_Level.

   --  Note that this description does not take into account the dynamic offset
   --  that may be added by Offset_Static_Level to an N_Integer_Literal node.

   type Level_T (S : Boolean := False) is record
      case S is
         when False => N : Node_Id;
         when True  => U : Uint;
      end case;
   end record;
   --  Result type of the Accessibility_Level function, containing an integer
   --  when Static is True or a Node_Id when Static is False, see below.

   function Accessibility_Level
     (Expr              : Node_Id;
      Level             : Accessibility_Level_Kind;
      Static            : Boolean := False;
      In_Return_Context : Boolean := False;
      Allow_Alt_Model   : Boolean := True) return Level_T;
   --  Centralized accessibility level calculation routine for finding the
   --  accessibility level of a given expression Expr. Level is as described
   --  above. If Static is True, the result is meant for a static context,
   --  typically a static accessibility check, and is therefore required to
   --  be a value of Level_T (S => True) subtype (which also means that Level
   --  cannot be Dynamic_Level in this case); otherwise, the result is meant
   --  for a dynamic context, typically a dynamic accessibility check, and is
   --  therefore required to be a value of Level_T (S => False) subtype.

   --  In_Return_Context forces the level calculation to be carried out as if
   --  Expr was an operative constituent of a return value; when it is False,
   --  the function computes whether that is the case or not.

   --  The Allow_Alt_Model parameter allows the alternative level calculation
   --  under the restriction No_Dynamic_Accessibility_Checks to be performed.

   procedure Apply_Accessibility_Check_For_Anonymous_Return
     (Exp         : Node_Id;
      Func        : Entity_Id;
      Insert_Node : Node_Id);
   --  If the result type of the function is an anonymous access type, insert
   --  a check that the accessibility level of the entity designated by the
   --  result is not deeper than the level of the master of the call. Exp is
   --  an expression being returned from Func.

   procedure Apply_Accessibility_Check_For_Class_Wide_Return
     (Exp  : Node_Id;
      Func : Entity_Id);
   --  Ada 2005 (AI95-344): If the result type is class-wide, insert a check
   --  that the level of the return expression's underlying type is not deeper
   --  than the level of the master enclosing the function. Always generate the
   --  check when the type of the return expression is class-wide, when it's a
   --  type conversion, or when it's a formal parameter. Otherwise suppress the
   --  check in the case where the return expression has a specific type whose
   --  level is known not to be statically deeper than the result type of the
   --  function. Exp is an expression being returned from Func.

   procedure Apply_Accessibility_Check_For_Discriminated_Return
     (Exp  : Node_Id;
      Func : Entity_Id);
   --  If the result type of the function has access discriminants, insert
   --  checks that the accessibility level of each entity designated by an
   --  access discriminant of the result is not deeper than the level of the
   --  master of the call. Exp is an expression being returned from Func.

   function Offset_Static_Level
     (Level : Uint;
      Subp  : Entity_Id) return Node_Id;
   --  Apply the offset scheme described for Extra_Accessibility_Of_Subprogram
   --  in Einfo to Level computed within subprogram Subp and return the result.
   --  The function must be invoked for every static accessibility level, both
   --  for entities declared, and master constructs present, within Subp, when
   --  a dynamic accessibility level is being computed.

   type Type_Level_T is record
      Base   : Entity_Id;
      Offset : Uint;
   end record;
   --  Result type of the Type_Access_Level function, containing a base entity
   --  and an offset. The static accessibility level of the input type is the
   --  sum of the local accessibility level of the base entity and the offset.

   function Type_Access_Level
     (Typ             : Entity_Id;
      Deepest         : Boolean := False;
      Allow_Alt_Model : Boolean := True;
      Assoc_Node      : Node_Id := Empty) return Type_Level_T;
   --  Return the static accessibility level of Typ

   --  When Deepest is True, and Typ is that of an Ada 2012 stand-alone object
   --  of an anonymous access type, then return the static accessibility level
   --  of the declaration of the object instead of the library level; moreover,
   --  in the case of a descendant of a generic formal type, return Int'Last
   --  instead of the library level.

   --  The Allow_Alt_Model parameter allows the alternative level calculation
   --  under the restriction No_Dynamic_Accessibility_Checks to be performed.

   --  Assoc_Node allows for the optional specification of a node associated
   --  with Typ. This is used only for anonymous access types where the context
   --  matters in interpreting Typ's level.

   ---------------------------
   -- Accessibility_Message --
   ---------------------------

   procedure Accessibility_Message (N : Node_Id; Typ : Entity_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      P     : constant Node_Id    := Prefix (N);
      Indic : Node_Id             := Parent (Parent (N));

   begin
      --  In an instance, this is a runtime check, but one we know will fail,
      --  so generate an appropriate warning. As usual, this kind of warning
      --  is an error in SPARK mode or if No_Dynamic_Accessibility_Checks.

      if In_Instance_Body then
         Error_Msg_Warn := SPARK_Mode /= On
                             and then not
                               No_Dynamic_Accessibility_Checks_Enabled (P);
         Error_Msg_F
           ("non-local pointer cannot point to local object<<", P);
         Error_Msg_F ("\Program_Error [<<", P);
         Rewrite (N,
           Make_Raise_Program_Error (Loc,
             Reason => PE_Accessibility_Check_Failed));
         Set_Etype (N, Typ);

      else
         Error_Msg_F ("non-local pointer cannot point to local object", P);

         --  Check for case where we have a missing access definition

         if Is_Record_Type (Current_Scope)
           and then
             Nkind (Parent (N)) in N_Discriminant_Association
                                 | N_Index_Or_Discriminant_Constraint
         then
            Indic := Parent (Parent (N));
            while Present (Indic)
              and then Nkind (Indic) /= N_Subtype_Indication
            loop
               Indic := Parent (Indic);
            end loop;

            if Present (Indic) then
               Error_Msg_NE
                 ("\use an access definition for" &
                  " the access discriminant of&",
                  N, Entity (Subtype_Mark (Indic)));
            end if;
         end if;
      end if;
   end Accessibility_Message;

   -------------------------
   -- Accessibility_Level --
   -------------------------

   function Accessibility_Level
     (Expr              : Node_Id;
      Level             : Accessibility_Level_Kind;
      Static            : Boolean := False;
      In_Return_Context : Boolean := False;
      Allow_Alt_Model   : Boolean := True) return Level_T
   is
      Loc : constant Source_Ptr := Sloc (Expr);

      function Accessibility_Level (Expr : Node_Id) return Level_T is
        (Accessibility_Level
          (Expr, Level, Static, In_Return_Context, Allow_Alt_Model));
      --  Renaming of the enclosing function to facilitate recursive calls

      function Enclosing_Master_Level (N : Node_Id) return Level_T;
      --  Return the accessibility level of the innermost enclosing master

      function Function_Call_Or_Allocator_Level (N : Node_Id) return Level_T;
      --  Centralized processing of subprogram calls which may appear in prefix
      --  notation.

      function Library_Level return Level_T;
      --  Return the library level

      function Local_Access_Level (E : Entity_Id) return Level_T is
        (if Static
         then (S => True,  U => Static_Local_Access_Level (E))
         else (S => False, N => Dynamic_Local_Access_Level (E)));
      --  Local renaming to select the appropriate variant for entities

      function Make_Level_Literal (Level : Uint) return Node_Id;
      --  Construct an integer literal representing an accessibility level with
      --  its type set to Natural.

      function Make_Level_Value (Level : Uint) return Level_T;
      --  Construct an accessibility level value with from static Level

      function Subprogram_Access_Level (Subp : Entity_Id) return Level_T is
        (if Static
         then (S => True,  U => Static_Subprogram_Access_Level (Subp))
         else (S => False, N => Dynamic_Subprogram_Access_Level (Subp)));
      --  Local renaming to select the appropriate variant for subprograms

      function Type_Access_Level (Typ : Entity_Id) return Level_T is
        (if Static
         then (S => True,
               U => Static_Type_Access_Level
                      (Typ, Allow_Alt_Model => Allow_Alt_Model))
         else (S => False,
               N => Dynamic_Type_Access_Level
                      (Typ, Allow_Alt_Model => Allow_Alt_Model)));
      --  Local renaming to select the appropriate variant for types

      ----------------------------
      -- Enclosing_Master_Level --
      ----------------------------

      function Enclosing_Master_Level (N : Node_Id) return Level_T is
         Encl_Scop      : Entity_Id;
         Ent            : Entity_Id;
         Level_Modifier : Int := 0;
         Node_Par       : Node_Id;

      begin
         --  Locate the nearest enclosing node (by traversing Parents)
         --  that Defining_Entity can be applied to, and return the
         --  level of that entity's nearest enclosing dynamic scope.

         --  The RM 7.6.1(3) definition of "master" includes statements
         --  and conditions for loops among other things. Are these cases
         --  detected properly ???

         Node_Par := Parent (N);
         while Present (Node_Par) loop
            Ent := Defining_Entity_Or_Empty (Node_Par);

            if Present (Ent) then
               Encl_Scop := Nearest_Dynamic_Scope (Ent);

               --  Ignore transient scopes made during expansion while also
               --  taking into account certain expansions - like iterators
               --  which get expanded into renamings and thus not marked
               --  as coming from source.

               if Comes_From_Source (Node_Par)
                 or else (Nkind (Node_Par) = N_Object_Renaming_Declaration
                           and then Comes_From_Iterator (Node_Par))
               then
                  --  Note that in some rare cases the scope depth may not be
                  --  set, for example, when we are in the middle of analyzing
                  --  a type and the enclosing scope is said type. In that case
                  --  simply return the library level.

                  if not Scope_Depth_Set (Encl_Scop) then
                     return Library_Level;
                  end if;

                  --  Handle the case of expressions within library level
                  --  subprograms here by adding one to the level modifier.

                  if Encl_Scop = Standard_Standard
                    and then Nkind (Node_Par) = N_Subprogram_Body
                  then
                     Level_Modifier := Level_Modifier + 1;
                  end if;

                  declare
                     Subp : constant Entity_Id :=
                       (if Is_Subprogram (Encl_Scop)
                        then Encl_Scop
                        else Enclosing_Subprogram (Encl_Scop));
                     Static_Level : constant Uint :=
                        Scope_Depth (Encl_Scop) + Level_Modifier;

                  begin
                     if Static then
                        return (S => True, U => Static_Level);
                     elsif No (Subp) then
                        return (S => False,
                                N => Make_Level_Literal (Static_Level));
                     else
                        return (S => False,
                                N => Offset_Static_Level (Static_Level, Subp));
                     end if;
                  end;
               end if;

            --  For a return statement within a function, return
            --  the depth of the function itself. This is not just
            --  a small optimization, but matters when analyzing
            --  the expression in an expression function before
            --  the body is created.

            elsif Nkind (Node_Par) in N_Extended_Return_Statement
                                    | N_Simple_Return_Statement
            then
               declare
                  Subp : constant Entity_Id :=
                    Enclosing_Subprogram (Node_Par);
                  Static_Level : constant Uint := Scope_Depth (Subp);

               begin
                  if Static then
                     return (S => True, U => Static_Level);
                  else
                     return (S => False,
                             N => Offset_Static_Level (Static_Level, Subp));
                  end if;
               end;

            --  Non-package bodies and statements are counted as masters

            elsif Nkind (Node_Par) in N_Entry_Body
                                    | N_Subprogram_Body
                                    | N_Task_Body
              or else Is_Statement (Node_Par)
            then
               Level_Modifier := Level_Modifier + 1;
            end if;

            Node_Par := Parent (Node_Par);
         end loop;

         --  Should never reach the following return

         pragma Assert (False);

         return Make_Level_Value (Scope_Depth (Current_Scope) + 1);
      end Enclosing_Master_Level;

      -------------------
      -- Library_Level --
      -------------------

      function Library_Level return Level_T is
      begin
         return Make_Level_Value (Scope_Depth (Standard_Standard));
      end Library_Level;

      ------------------------
      -- Make_Level_Literal --
      ------------------------

      function Make_Level_Literal (Level : Uint) return Node_Id is
         Result : constant Node_Id := Make_Integer_Literal (Loc, Level);

      begin
         Set_Etype (Result, Standard_Natural);
         return Result;
      end Make_Level_Literal;

      ----------------------
      -- Make_Level_Value --
      ----------------------

      function Make_Level_Value (Level : Uint) return Level_T is
      begin
         if Static then
            return (S => True, U => Level);
         else
            return (S => False, N => Make_Level_Literal (Level));
         end if;
      end Make_Level_Value;

      --------------------------------------
      -- Function_Call_Or_Allocator_Level --
      --------------------------------------

      function Function_Call_Or_Allocator_Level (N : Node_Id) return Level_T is
         Prev_Par : Node_Id := Expr;
         Par      : Node_Id := Parent (Expr);
         --  Par and Prev_Par will be used for traversing the AST, while
         --  maintaining an invariant that Par = Parent (Prev_Par).

      begin
         --  First deal with function calls in Ada 95

         if Nkind (N) = N_Function_Call
           and then Ada_Version <= Ada_95
         then
            --  With a return by reference, we either get the accessibility of
            --  the function or, in case of an indirect call, the accessibility
            --  level of the access-to-subprogram type.

            if Is_Entity_Name (Name (N))
              and then Is_Inherently_Limited_Type (Etype (N))
            then
               return Subprogram_Access_Level (Entity (Name (N)));

            elsif Nkind (Name (N)) = N_Explicit_Dereference
              and then Is_Inherently_Limited_Type (Etype (N))
            then
               return Type_Access_Level (Etype (Prefix (Name (N))));

            --  Otherwise the accessibility level of the innermost master

            else
               return Enclosing_Master_Level (Expr);
            end if;

         --  We ignore coextensions as they cannot be implemented under the
         --  "small integer" model.

         elsif Nkind (N) = N_Allocator
           and then (Is_Static_Coextension (N)
                      or else Is_Dynamic_Coextension (N))
         then
            return Library_Level;

         --  Objects of a named access type get their level from their type

         elsif Is_Named_Access_Type (Etype (N)) then
            return Type_Access_Level (Etype (N));

         --  Function calls in Ada 2005 and later, and anonymous allocators

         else
            --  Check No_Dynamic_Accessibility_Checks restriction override for
            --  alternative accessibility model.

            if Allow_Alt_Model
              and then No_Dynamic_Accessibility_Checks_Enabled (N)
              and then Is_Anonymous_Access_Type (Etype (N))
            then
               --  In the alternative model the level is that of the
               --  designated type.

               if Debug_Flag_Underscore_B then
                  return
                    Make_Level_Value (Static_Type_Access_Level (Etype (N)));

               --  For function calls the level is that of the innermost
               --  master; otherwise, for allocators we get the level of
               --  the corresponding anonymous access type, which is
               --  calculated through the normal path of execution.

               elsif Nkind (N) = N_Function_Call then
                  return Enclosing_Master_Level (Expr);
               end if;
            end if;

            --  AI12-0402: The master of the function call for a function
            --  whose result type is a scalar type is always the innermost
            --  master invoking the function.

            if Ada_Version >= Ada_2022
              and then Nkind (N) = N_Function_Call
              and then Is_Scalar_Type (Etype (N))
            then
               return Enclosing_Master_Level (Expr);
            end if;

            --  RM 3.10.2(10.5): If the call itself defines the result of a
            --  function F, or has an accessibility level that is tied to the
            --  result of such a function F, then the master of the call is
            --  that of the master of the call invoking F.

            if In_Return_Context or In_Return_Value (N) then
               declare
                  Extra_Formal : constant Entity_Id :=
                    Extra_Accessibility_Of_Result (Current_Subprogram);

               begin
                  --  If a function is passed an extra "level of the
                  --  master of the call" parameter and that function
                  --  returns a call to another such function (or
                  --  possibly to the same function, in the case of a
                  --  recursive call), then that parameter should be
                  --  "passed along".

                  if Present (Extra_Formal) and then Level = Dynamic_Level then
                     return (S => False,
                             N => New_Occurrence_Of (Extra_Formal, Loc));

                  --  Otherwise, return accessibility level of the enclosing
                  --  subprogram.

                  else
                     return Subprogram_Access_Level (Current_Subprogram);
                  end if;
               end;
            end if;

            --  Find any relevant parent nodes that designate an object being
            --  initialized and stop when there is an inappropriate construct.

            while Present (Par) loop

               case Nkind (Par) is

                  --  RM 3.10.2 (10.2/5) is relevant only if the result is used
                  --  to "directly initialize" the object.

                  when N_Explicit_Dereference | N_Function_Call | N_Op =>
                     exit;

                  --  RM 3.10.2 (10.2/5) is relevant only if the result is used
                  --  "in its entirety".

                  when N_Indexed_Component | N_Selected_Component | N_Slice =>
                     exit;

                  --  Accept operative constituents

                  when N_Case_Expression =>
                     exit when Prev_Par = Expression (Par);

                  when N_If_Expression =>
                     exit when Prev_Par = First (Expressions (Par));

                  when N_Case_Expression_Alternative
                     | N_Qualified_Expression
                     | N_Unchecked_Type_Conversion
                  =>
                     null;

                  --  Detect an expanded implicit conversion, typically this
                  --  occurs on implicitly converted actuals in calls.

                  --  Does this catch all implicit conversions ???

                  when N_Type_Conversion  =>
                     if Is_Named_Access_Type (Etype (Par)) then
                        return Type_Access_Level (Etype (Par));
                     end if;

                  --  For the (static) declaration of an object, return the
                  --  accessibility level of the master of the object.

                  when N_Object_Declaration =>
                     return Accessibility_Level
                              (Expr   => Defining_Identifier (Par),
                               Level  => Object_Decl_Level,
                               Static => Static);

                  --  For the dynamic allocation of an object, return the
                  --  accessibility level of the allocator.

                  when N_Allocator =>
                     return Accessibility_Level (Par);

                  --  RM 3.10.2(10.3/5): If the result is of an anonymous
                  --  access type and is converted to a (named or anonymous)
                  --  access type, the master is determined following the
                  --  rules given for determining the master of an object
                  --  created by an allocator.

                  --  The conversion can be an implicit subtype conversion,
                  --  in particular the one in an assignment (RM 5.2(11/5)).

                  --  For an anonymous allocator in an assignment, return the
                  --  accessibility level of the name (RM 3.10.2(14/6)).

                  when N_Assignment_Statement =>
                     exit when Prev_Par = Name (Par)
                       or else not Is_Anonymous_Access_Type (Etype (N));

                     return Accessibility_Level
                              (Expr              => Name (Par),
                               Level             => Object_Decl_Level,
                               Static            => Static,
                               In_Return_Context => In_Return_Context);

                  when others =>
                     --  Prevent the search from going too far

                     exit when Is_Statement (Par)
                       or else Is_Body_Or_Package_Declaration (Par);
               end case;

               Prev_Par := Par;
               Par := Parent (Par);
            end loop;

            --  Return the accessibility level of the innermost master

            return Enclosing_Master_Level (Expr);
         end if;
      end Function_Call_Or_Allocator_Level;

      --  Local variables

      E   : Node_Id;
      Pre : Node_Id;

   --  Start of processing for Accessibility_Level

   begin
      --  Make sure that we are not passed contradictory input

      pragma Assert (not (Level = Dynamic_Level and then Static));

      --  We could be looking at a reference to a formal due to the expansion
      --  of entries and other cases, so obtain the renaming if necessary.

      if Present (Param_Entity (Expr)) then
         E := Param_Entity (Expr);

      --  Use the original node unless it is an unanalyzed identifier, as we
      --  don't want to reason on unanalyzed expressions from predicates, or
      --  a folded conditional expression, since its level is ultimately that
      --  of the dependent expression evaluated statically in this case.

      elsif not (Nkind (Original_Node (Expr)) = N_Identifier
                  and then not Analyzed (Original_Node (Expr)))
        and then not (Nkind (Original_Node (Expr)) = N_Case_Expression
                       and then
                         Is_Static_Expression
                           (Expression (Original_Node (Expr))))
        and then not (Nkind (Original_Node (Expr)) = N_If_Expression
                       and then
                         Is_Static_Expression
                           (First (Expressions (Original_Node (Expr)))))
      then
         E := Original_Node (Expr);

      else
         E := Expr;
      end if;

      --  Extract the entity when it is valid

      if Nkind (E) in N_Has_Entity
        and then Present (Entity (E))
        and then Entity (E) /= Any_Type
      then
         E := Entity (E);

         --  Deal with a possible renaming of a private protected component

         if Ekind (E) in E_Constant | E_Variable and then Is_Prival (E) then
            E := Prival_Link (E);
         end if;
      end if;

      --  Perform the processing on the expression

      case Nkind (E) is
         --  The accessibility level of the literal null is the library level

         when N_Null =>
            return Library_Level;

         --  The level of an aggregate is that of the innermost master that
         --  evaluates it as defined in RM 3.10.2 (10/4).

         when N_Aggregate =>
            return Enclosing_Master_Level (Expr);

         --  The accessibility level is that of the access type, except for
         --  anonymous allocators which have special rules defined in RM 3.10.2
         --  (14/3).

         when N_Allocator =>
            return Function_Call_Or_Allocator_Level (E);

         --  We could reach this point for two reasons. Either the expression
         --  applies to a special attribute ('Loop_Entry, 'Result, or 'Old), or
         --  we are looking at the access attributes directly ('Access,
         --  'Address, or 'Unchecked_Access).

         when N_Attribute_Reference =>
            Pre := Original_Node (Prefix (E));

            --  Regular 'Access attribute presence means we have to look at the
            --  prefix.

            if Attribute_Name (E) = Name_Access then
               return Accessibility_Level (Prefix (E));

            --  If we have reached a 'Input attribute then this is the
            --  the result of the expansion of an object declaration with
            --  an initial value featuring it. Is this the only case ???

            --  For example:

            --    Opaque : aliased Stream_Element_Array :=
            --      Stream_Element_Array'Input (S);

            elsif Attribute_Name (E) = Name_Input then

               --  Return the level of the enclosing declaration

               return
                 Enclosing_Master_Level (Enclosing_Declaration (Expr));

            --  Return the library level to null out the check for the Address,
            --  Deref, Unchecked_Access and Unrestricted_Access attributes.

            elsif Attribute_Name (E) in Name_Address
                                      | Name_Deref
                                      | Name_Unchecked_Access
                                      | Name_Unrestricted_Access
            then
               return Library_Level;

            --  For 'Old return the level of the associated entity, if any

            elsif Attribute_Name (E) = Name_Old
              and then Is_Entity_Name (Expr)
            then
               return Accessibility_Level (Entity (Expr));

            --  'Access can be taken further against other special attributes,
            --  so handle these cases explicitly.

            elsif Attribute_Name (E)
                    in Name_Old        |
                       Name_Loop_Entry |
                       Name_Result     |
                       Name_Super      |
                       Name_Tag        |
                       Name_Safe_First |
                       Name_Safe_Last  |
                       Name_First      |
                       Name_Last
            then
               --  Named access types

               if Is_Named_Access_Type (Etype (Pre)) then
                  return Type_Access_Level (Etype (Pre));

               --  Anonymous access types

               elsif Is_Entity_Name (Pre)
                 and then Ekind (Entity (Pre)) not in Subprogram_Kind
                 and then Present (Extra_Accessibility (Entity (Pre)))
                 and then Level = Dynamic_Level
               then
                  pragma Assert (Is_Anonymous_Access_Type (Etype (Pre)));
                  return (S => False,
                          N => New_Occurrence_Of
                                 (Extra_Accessibility (Entity (Pre)), Loc));

               --  Otherwise the level is treated in a similar way as
               --  aggregates according to RM 6.1.1 (35.1/4) which concerns
               --  an implicit constant declaration - in turn defining the
               --  accessibility level to be that of the implicit constant
               --  declaration.

               else
                  return Enclosing_Master_Level (Expr);
               end if;

            else
               raise Program_Error;
            end if;

         --  This is the "base case" for accessibility level calculations which
         --  means we are near the end of our recursive traversal.

         when N_Defining_Identifier =>
            --  RM 3.10.2(21.1/5): Notwithstanding other rules [in 3.10.2],
            --  the accessibility level of an entity that is tied to that of
            --  an explicitly aliased formal parameter of an enclosing function
            --  is considered (both statically and dynamically) to be the same
            --  as that of an entity whose accessibility level is tied to that
            --  of the return object of that function.

            --  This means that no checks are needed for an explicitly aliased
            --  formal parameter in a return context and we return the library
            --  level to null them out there.

            --  Note that we have to deal specifically with _Wrapped_Statements
            --  functions of functions returning an access result, generated by
            --  the expansion of contracts and postconditions, because they get
            --  the same anonymous access result type as their parent function.

            if Is_Explicitly_Aliased (E)
              and then (Scope (E) = Current_Subprogram
                         or else (Has_Expanded_Contract (Scope (E))
                                   and then
                                     Wrapped_Statements (Scope (E)) =
                                                           Current_Subprogram))
              and then (In_Return_Context or else In_Return_Value (Expr))
            then
               return Library_Level;

            --  Formal parameters with an extra accessibility formal, as well
            --  as stand-alone objects of an anonymous access type (SAOOAAAT).

            elsif (Is_Formal (E) or else Ekind (E) in E_Constant | E_Variable)
              and then Present (Extra_Accessibility (E))
              and then Level in Dynamic_Level | Library_On_Dynamic_Level
            then
               --  Return the library level if explicitly requested

               if Level = Library_On_Dynamic_Level then
                  return Library_Level;

               --  No_Dynamic_Accessibility_Checks restriction override for
               --  alternative accessibility model.

               elsif Allow_Alt_Model
                 and then No_Dynamic_Accessibility_Checks_Enabled (E)
               then
                  --  In the alternative model the level is that of the
                  --  designated type entity's context.

                  if Debug_Flag_Underscore_B then
                     return Make_Level_Value
                              (Static_Type_Access_Level (Etype (E)));
                  else
                     return Make_Level_Value (Static_Local_Access_Level (E));
                  end if;

               --  Return the dynamic level in the normal case

               else
                  return
                    (S => False,
                     N => New_Occurrence_Of (Extra_Accessibility (E), Loc));
               end if;

            --  Return the library level for formal parameters or stand-alone
            --  objects of an anonymous access type without extra accessibility
            --  object. This may happen for subprograms with foreign convention
            --  or when expansion is disabled, see Sem_Ch6.Create_Extra_Formals
            --  and Exp_Ch3.Expand_N_Object_Declaration.

            elsif (Is_Formal (E) or else Ekind (E) in E_Constant | E_Variable)
              and then Is_Anonymous_Access_Type (Etype (E))
              and then not Is_Local_Anonymous_Access (Etype (E))
              and then Level in Dynamic_Level | Library_On_Dynamic_Level
            then
               return Library_Level;

            --  Initialization procedures have a special extra accessibility
            --  parameter associated with the level at which the object
            --  being initialized exists

            elsif Level = Dynamic_Level
              and then Ekind (E) = E_Record_Type
              and then Is_Limited_Record (E)
              and then Current_Scope = Init_Proc (E)
              and then Present (Init_Proc_Level_Formal (Current_Scope))
            then
               return
                 (S => False,
                  N => New_Occurrence_Of
                         (Init_Proc_Level_Formal (Current_Scope), Loc));

            --  Current instance of the type is deeper than that of the type
            --  according to RM 3.10.2 (21).

            elsif Is_Type (E) then
               --  When restriction No_Dynamic_Accessibility_Checks is active
               --  along with -gnatd_b.

               if Allow_Alt_Model
                 and then No_Dynamic_Accessibility_Checks_Enabled (E)
                 and then Debug_Flag_Underscore_B
               then
                  return Make_Level_Value (Static_Type_Access_Level (E));
               end if;

               --  Normal path

               if Static then
                  return (S => True, U => Static_Type_Access_Level (E) + 1);
               else
                  return
                    (S => False,
                     N => Make_Op_Add (Loc,
                            Left_Opnd  => Dynamic_Type_Access_Level (E),
                            Right_Opnd => Make_Level_Literal (Uint_1)));
               end if;

            --  Move up the renamed entity or object if it came from source
            --  since expansion may have created a dummy renaming under
            --  certain circumstances.

            --  Note: We check if the original node of the renaming comes
            --  from source because the node may have been rewritten.

            elsif Present (Renamed_Entity_Or_Object (E))
              and then Comes_From_Source
                (Original_Node (Renamed_Entity_Or_Object (E)))
            then
               return Accessibility_Level (Renamed_Entity_Or_Object (E));

            --  Objects of a named access type get their level from their type

            elsif Is_Named_Access_Type (Etype (E)) then
               return Type_Access_Level (Etype (E));

            --  Check if E is an expansion-generated renaming of an iterator
            --  by examining Related_Expression. If so, determine the
            --  accessibility level based on the original expression.

            elsif Ekind (E) in E_Constant | E_Variable
              and then Present (Related_Expression (E))
            then
               return Accessibility_Level (Related_Expression (E));

            elsif Level = Dynamic_Level
               and then Ekind (E) in E_In_Parameter | E_In_Out_Parameter
               and then Is_Subprogram (Scope (E))
               and then Present (Init_Proc_Level_Formal (Scope (E)))
            then
               return
                 (S => False,
                  N => New_Occurrence_Of
                         (Init_Proc_Level_Formal (Scope (E)), Loc));

            --  Formal object of generic subprogram - get the level of the
            --  subprogram

            elsif Is_Formal_Object (E) and then Is_Subprogram (Scope (E)) then
               return Subprogram_Access_Level (Scope (E));

            --  Normal object - get the local access level

            else
               return Local_Access_Level (E);
            end if;

         --  Handle indexed and selected components including the special cases
         --  whereby there is an implicit dereference, a component of a
         --  composite type, or a function call in prefix notation.

         when N_Indexed_Component | N_Selected_Component | N_Slice =>
            Pre := Prefix (E);

            --  Fetch the original node when the prefix comes from the result
            --  of expanding a function call since we want to find the level
            --  of the original source call.

            if not Comes_From_Source (Pre) then
               if Nkind (Original_Node (Pre)) = N_Function_Call then
                  Pre := Original_Node (Pre);
               elsif Is_Captured_Function_Call (Pre) then
                  Pre := Prefix (Constant_Value (Entity (Prefix (Pre))));
               end if;
            end if;

            --  When E is an indexed component or selected component and
            --  the current Expr is a function call, we know that we are
            --  looking at an expanded call in prefix notation.

            if Nkind (Expr) = N_Function_Call then
               return Function_Call_Or_Allocator_Level (Expr);

            --  If the prefix is a named access type, then we are dealing
            --  with an implicit deferences. In that case the level is that
            --  of the named access type in the prefix.

            elsif Is_Named_Access_Type (Etype (Pre)) then
               return Type_Access_Level (Etype (Pre));

            --  The current expression is a named access type, so there is no
            --  reason to look at the prefix. Instead obtain the level of E's
            --  named access type.

            elsif Is_Named_Access_Type (Etype (E)) then
               return Type_Access_Level (Etype (E));

            --  A nondiscriminant selected component where the component
            --  is an anonymous access type means that its associated
            --  level is that of the containing type - see RM 3.10.2 (16).

            --  Note that when restriction No_Dynamic_Accessibility_Checks is
            --  in effect we treat discriminant components as regular
            --  components.

            elsif
              (Nkind (E) = N_Selected_Component
                and then Ekind (Etype (E))   =  E_Anonymous_Access_Type
                and then Ekind (Etype (Pre)) /= E_Anonymous_Access_Type
                and then (not (Nkind (Selector_Name (E)) in N_Has_Entity
                                and then Ekind (Entity (Selector_Name (E)))
                                           = E_Discriminant)

                           --  The alternative accessibility models both treat
                           --  discriminants as regular components.

                           or else (No_Dynamic_Accessibility_Checks_Enabled (E)
                                     and then Allow_Alt_Model)))

              --  Arrays featuring components of anonymous access components
              --  get their corresponding level from their containing type's
              --  declaration.

              or else
                (Nkind (E) = N_Indexed_Component
                  and then Ekind (Etype (E)) = E_Anonymous_Access_Type
                  and then Ekind (Etype (Pre)) in Array_Kind
                  and then Ekind (Component_Type (Base_Type (Etype (Pre))))
                             = E_Anonymous_Access_Type)
            then
               --  When restriction No_Dynamic_Accessibility_Checks is active
               --  and -gnatd_b set, the level is that of the designated type.

               if Allow_Alt_Model
                 and then No_Dynamic_Accessibility_Checks_Enabled (E)
                 and then Debug_Flag_Underscore_B
               then
                  return
                    Make_Level_Value (Static_Type_Access_Level (Etype (E)));
               end if;

               --  Otherwise proceed normally

               return Type_Access_Level (Etype (Prefix (E)));

            --  The accessibility calculation routine that handles function
            --  calls (Function_Call_Or_Allocator_Level) assumes, in the case
            --  the result is not of a named access type, that the result will
            --  be used "in its entirety" when the call is present within an
            --  assignment or object declaration or return value.

            --  To properly handle cases where the result is not used in its
            --  entirety, we test if the prefix of the component in question is
            --  a function call, which tells us that one of its components has
            --  been identified and is being accessed.

            elsif Nkind (Pre) = N_Function_Call
              and then not Is_Named_Access_Type (Etype (Pre))
            then
               --  Even if the result is not used "in it entirety", if the call
               --  has an accessibility level tied to that of the result of the
               --  enclosing function, it is nevertheless considered to define
               --  the result for the purpose of determining its master and its
               --  accessibility level by the RM 3.10.2(10.5/5) rule.

               if (Ekind (Etype (Pre)) = E_Anonymous_Access_Type
                    or else Has_Implicit_Dereference (Etype (Pre)))
                 and then (In_Return_Context or else In_Return_Value (E))
               then
                  return Function_Call_Or_Allocator_Level (Prefix (E));
               end if;

               return Enclosing_Master_Level (Expr);

            --  Otherwise, continue recursing over the expression prefixes

            else
               return Accessibility_Level (Prefix (E));
            end if;

         --  Conditional expressions: the accessibility level is the level of
         --  the evaluated dependent expression (RM 3.10.2(9.1)).

         when N_Case_Expression =>
            --  If the expression is static, perform a static computation

            if Is_Static_Expression (Expression (E)) then
               declare
                  Alt : Node_Id;

               begin
                  Alt := First (Alternatives (E));
                  while Present (Alt) loop
                     if Choices_Match (Expression (E), Discrete_Choices (Alt))
                       = Match
                     then
                        return Accessibility_Level (Expression (Alt));
                     end if;

                     Next (Alt);
                  end loop;

                  raise Program_Error;
               end;

            --  Or else, the expression is dynamic so, if the level is also
            --  dynamic, we can perform a dynamic computation.

            elsif Level = Dynamic_Level then
               declare
                  New_Alts : constant List_Id := New_List;
                  New_Case : constant Node_Id :=
                    Make_Case_Expression (Loc,
                      Expression   => Duplicate_Subexpr (Expression (E)),
                      Alternatives => New_Alts);

                  Alt : Node_Id;

               begin
                  Alt := First (Alternatives (E));
                  while Present (Alt) loop
                     Append_To (New_Alts,
                       Make_Case_Expression_Alternative (Sloc (Alt),
                         Discrete_Choices => Discrete_Choices (Alt),
                         Expression       =>
                           Accessibility_Level (Expression (Alt)).N));

                     Next (Alt);
                  end loop;

                  return (S => False, N => New_Case);
               end;

            --  Otherwise, the expression is dynamic but the level is static,
            --  so return the library level to null out the check.

            else
               pragma Assert (Level = Library_On_Dynamic_Level);
               return Library_Level;
            end if;

         when N_If_Expression =>
            declare
               Condition  : constant Node_Id := First (Expressions (E));
               Then_Expr  : constant Node_Id := Next (Condition);
               Else_Expr  : constant Node_Id := Next (Then_Expr);

            begin
               --  If the condition is static, perform a static computation

               if Is_Static_Expression (Condition) then
                  return
                    (if Is_True (Expr_Value (Condition))
                     then Accessibility_Level (Then_Expr)
                     else Accessibility_Level (Else_Expr));

               --  Or else, the condition is dynamic so, if the level is also
               --  dynamic, we can perform a dynamic computation.

               elsif Level = Dynamic_Level then
                  return
                    (S => False,
                     N => Make_If_Expression (Loc,
                            Expressions => New_List (
                              Duplicate_Subexpr (Condition),
                              Accessibility_Level (Then_Expr).N,
                              Accessibility_Level (Else_Expr).N)));

               --  Otherwise, the condition is dynamic but the level is static,
               --  so return the library level to null out the check.

               else
                  pragma Assert (Level = Library_On_Dynamic_Level);
                  return Library_Level;
               end if;
            end;

         --  Qualified expressions

         when N_Qualified_Expression =>
            if Is_Named_Access_Type (Etype (E)) then
               return Type_Access_Level (Etype (E));
            else
               return Accessibility_Level (Expression (E));
            end if;

         --  Handle function calls

         when N_Function_Call =>
            return Function_Call_Or_Allocator_Level (E);

         --  Explicit dereference accessibility level calculation

         when N_Explicit_Dereference =>
            Pre := Original_Node (Prefix (E));

            --  The prefix is a named access type so the level is taken from
            --  its type.

            if Is_Named_Access_Type (Etype (Pre)) then
               return Type_Access_Level (Etype (Pre));

            --  Otherwise, recurse deeper

            else
               return Accessibility_Level (Prefix (E));
            end if;

         --  Type conversions

         when N_Type_Conversion | N_Unchecked_Type_Conversion =>
            --  View conversions are special in that they require use to
            --  inspect the expression of the type conversion.

            --  Allocators of anonymous access types are internally generated,
            --  so recurse deeper in that case as well.

            if Is_View_Conversion (E)
              or else Ekind (Etype (E)) = E_Anonymous_Access_Type
            then
               return Accessibility_Level (Expression (E));

            --  We don't care about the master if we are looking at a named
            --  access type.

            elsif Is_Named_Access_Type (Etype (E)) then
               return Type_Access_Level (Etype (E));

            --  In section RM 3.10.2 (10/4) the accessibility rules for
            --  aggregates and value conversions are outlined. Are these
            --  followed in the case of initialization of an object ???

            --  Should use Enclosing_Master_Level ???

            else
               return Accessibility_Level (Current_Scope);
            end if;

         --  Default to the type accessibility level for the type of the
         --  expression's entity.

         when others =>
            return Type_Access_Level (Etype (E));
      end case;
   end Accessibility_Level;

   --------------------------------------------------------
   -- Apply_Accessibility_Check_For_Class_Wide_Allocator --
   --------------------------------------------------------

   procedure Apply_Accessibility_Check_For_Class_Wide_Allocator
     (N              : Node_Id;
      Exp            : Node_Id;
      Ref            : Node_Id;
      Built_In_Place : Boolean := False)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      PtrT      : constant Entity_Id  := Etype (N);
      DesigT    : constant Entity_Id  := Designated_Type (PtrT);
      Pool_Id   : constant Entity_Id  := Associated_Storage_Pool (PtrT);
      Cond      : Node_Id;
      Fin_Call  : Node_Id;
      Free_Stmt : Node_Id;
      Obj_Ref   : Node_Id;
      Stmts     : List_Id;

   begin
      if Ada_Version >= Ada_2005
        and then Is_Class_Wide_Type (DesigT)
        and then Tagged_Type_Expansion
        and then not Scope_Suppress.Suppress (Accessibility_Check)
        and then not No_Dynamic_Accessibility_Checks_Enabled (Ref)
        and then
          (Static_Type_Access_Level (Etype (Exp))
             > Static_Type_Access_Level (PtrT)
            or else
              (Is_Class_Wide_Type (Etype (Exp))
                and then Scope (PtrT) /= Current_Scope))
      then
         --  If the allocator was built in place, Ref is already a reference
         --  to the access object initialized to the result of the allocator
         --  (see Exp_Ch6.Make_Build_In_Place_Call_In_Allocator). We call
         --  Duplicate_Subexpr for cases where the build-in-place call may
         --  still be the prefix of the reference (to avoid generating
         --  duplicate calls). Otherwise, it is the entity associated with
         --  the object containing the address of the allocated object.

         if Built_In_Place then
            Obj_Ref := Duplicate_Subexpr (Ref);
         else
            Obj_Ref := New_Occurrence_Of (Ref, Loc);
         end if;

         --  For access to interface types we must generate code to displace
         --  the pointer to the base of the object since the subsequent code
         --  references components located in the TSD of the object (which
         --  is associated with the primary dispatch table --see a-tags.ads)
         --  and also generates code invoking Free, which requires also a
         --  reference to the base of the unallocated object.

         if Is_Interface (DesigT) and then Tagged_Type_Expansion then
            Obj_Ref :=
              Unchecked_Convert_To (Etype (Obj_Ref),
                Make_Function_Call (Loc,
                  Name                   =>
                    New_Occurrence_Of (RTE (RE_Base_Address), Loc),
                  Parameter_Associations => New_List (
                    Unchecked_Convert_To (RTE (RE_Address),
                      New_Copy_Tree (Obj_Ref)))));
         end if;

         --  Step 1: Create the object clean up code

         Stmts := New_List;

         --  Deallocate the object if the accessibility check fails. This is
         --  done only on targets or profiles that support deallocation.

         --    Free (Obj_Ref);

         if RTE_Available (RE_Free) then
            Free_Stmt := Make_Free_Statement (Loc, New_Copy_Tree (Obj_Ref));
            Set_Storage_Pool (Free_Stmt, Pool_Id);

            Append_To (Stmts, Free_Stmt);

         --  The target or profile cannot deallocate objects

         else
            Free_Stmt := Empty;
         end if;

         --  Finalize the object if applicable. Generate:

         --    [Deep_]Finalize (Obj_Ref.all);

         if Needs_Finalization (DesigT)
           and then not No_Heap_Finalization (PtrT)
         then
            Fin_Call :=
              Make_Final_Call
                (Obj_Ref =>
                   Make_Explicit_Dereference (Loc, New_Copy (Obj_Ref)),
                 Typ     => DesigT);

            --  Guard against a missing [Deep_]Finalize when the designated
            --  type was not properly frozen.

            if No (Fin_Call) then
               Fin_Call := Make_Null_Statement (Loc);
            end if;

            --  When the target or profile supports deallocation, wrap the
            --  finalization call in a block to ensure proper deallocation even
            --  if finalization fails. Generate:

            --    begin
            --       <Fin_Call>
            --    exception
            --       when others =>
            --          <Free_Stmt>
            --          raise;
            --    end;

            if Present (Free_Stmt) then
               Fin_Call :=
                 Make_Block_Statement (Loc,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (Fin_Call),

                     Exception_Handlers => New_List (
                       Make_Exception_Handler (Loc,
                         Exception_Choices => New_List (
                           Make_Others_Choice (Loc)),
                         Statements        => New_List (
                           New_Copy_Tree (Free_Stmt),
                           Make_Raise_Statement (Loc))))));
            end if;

            Prepend_To (Stmts, Fin_Call);
         end if;

         --  Signal the accessibility failure through a Program_Error

         Append_To (Stmts,
           Make_Raise_Program_Error (Loc,
             Reason => PE_Accessibility_Check_Failed));

         --  Step 2: Create the accessibility comparison

         --  Generate:
         --    Ref'Tag

         Obj_Ref :=
           Make_Attribute_Reference (Loc,
             Prefix         => Obj_Ref,
             Attribute_Name => Name_Tag);

         --  For tagged types, determine the accessibility level by looking at
         --  the type specific data of the dispatch table. Generate:

         --    Type_Specific_Data (Address (Ref'Tag)).Access_Level

         if Tagged_Type_Expansion then
            Cond := Build_Get_Access_Level (Loc, Obj_Ref);

         --  Use a runtime call to determine the accessibility level when
         --  compiling on virtual machine targets. Generate:

         --    Get_Access_Level (Ref'Tag)

         else
            Cond :=
              Make_Function_Call (Loc,
                Name                   =>
                  New_Occurrence_Of (RTE (RE_Get_Access_Level), Loc),
                Parameter_Associations => New_List (Obj_Ref));
         end if;

         Cond :=
           Make_Op_Gt (Loc,
             Left_Opnd  => Cond,
             Right_Opnd => Dynamic_Accessibility_Level (N));

         --  Due to the complexity and side effects of the check, utilize an if
         --  statement instead of the regular Program_Error circuitry.

         Insert_Action (N,
           Make_Implicit_If_Statement (N,
             Condition       => Cond,
             Then_Statements => Stmts),
           Suppress => All_Checks);
      end if;
   end Apply_Accessibility_Check_For_Class_Wide_Allocator;

   ----------------------------------------------------
   -- Apply_Accessibility_Check_For_Anonymous_Return --
   ----------------------------------------------------

   procedure Apply_Accessibility_Check_For_Anonymous_Return
     (Exp         : Node_Id;
      Func        : Entity_Id;
      Insert_Node : Node_Id)
   is
      Loc : constant Source_Ptr := Sloc (Exp);

      function Has_Level_Tied_To_Explicitly_Aliased_Parameter
        (Exp : Node_Id) return Boolean;
      --  Exp is an anonymous access value. Return True iff the accessibility
      --  of the type of Exp is the level of an explicitly aliased parameter
      --  of Func. If true, this indicates that no check should be performed
      --  for Exp.

      -----------------------------------------------------
      --  Has_Level_Tied_To_Explicitly_Aliased_Parameter --
      -----------------------------------------------------

      function Has_Level_Tied_To_Explicitly_Aliased_Parameter
        (Exp : Node_Id) return Boolean
      is
         E, P : Node_Id;

      begin
         E := Exp;

         --  Look through constants

         while Is_Entity_Name (E)
           and then Ekind (Entity (E)) = E_Constant
           and then Present (Constant_Value (Entity (E)))
         loop
            E := Constant_Value (Entity (E));
         end loop;

         if Nkind (E) = N_Attribute_Reference
           and then Get_Attribute_Id (Attribute_Name (E)) = Attribute_Access
         then
            P := Ultimate_Prefix (E);
            if Is_Entity_Name (P)
              and then Is_Explicitly_Aliased (Entity (P))
              and then Scope (Entity (P)) = Func
            then
               return True;
            end if;
         end if;

         return False;
      end Has_Level_Tied_To_Explicitly_Aliased_Parameter;

   --  Start of processing for Apply_Accessibility_Check_For_Anonymous_Return

   begin
      if Present (Extra_Accessibility_Of_Result (Func))
        and then not Has_Level_Tied_To_Explicitly_Aliased_Parameter (Exp)
      then
         Insert_Action (Insert_Node,
           Make_Raise_Program_Error (Loc,
             Condition =>
               Make_Op_Gt (Loc,
                 Left_Opnd  =>
                   Dynamic_Accessibility_Level
                     (Exp, In_Return_Context => True),
                 Right_Opnd => New_Occurrence_Of
                   (Extra_Accessibility_Of_Result (Func), Loc)),
             Reason    => PE_Accessibility_Check_Failed),
           Suppress => Access_Check);
      end if;
   end Apply_Accessibility_Check_For_Anonymous_Return;

   -----------------------------------------------------
   -- Apply_Accessibility_Check_For_Class_Wide_Return --
   -----------------------------------------------------

   procedure Apply_Accessibility_Check_For_Class_Wide_Return
     (Exp  : Node_Id;
      Func : Entity_Id)
   is
      Loc : constant Source_Ptr := Sloc (Exp);

   begin
       --  CodePeer does not do anything useful on Ada.Tags.Type_Specific_Data
       --  components.

      if Ada_Version >= Ada_2005
        and then not CodePeer_Mode
        and then Tagged_Type_Expansion
        and then
          (Is_Class_Wide_Type (Etype (Exp))
            or else Nkind (Exp) in
                      N_Type_Conversion | N_Unchecked_Type_Conversion
            or else (Is_Entity_Name (Exp)
                      and then Is_Formal (Entity (Exp)))
            or else Static_Type_Access_Level (Etype (Exp))
                      > Static_Subprogram_Access_Level (Func))
      then
         declare
            Tag_Node : Node_Id;

         begin
            --  Ada 2005 (AI-251): In class-wide interface objects we displace
            --  "this" to reference the base of the object. This is required to
            --  get access to the TSD of the object.

            if Is_Class_Wide_Type (Etype (Exp))
              and then Is_Interface (Etype (Exp))
            then
               --  If the expression is an explicit dereference then we can
               --  directly displace the pointer to reference the base of
               --  the object.

               if Nkind (Exp) = N_Explicit_Dereference then
                  Tag_Node :=
                    Make_Explicit_Dereference (Loc,
                      Prefix =>
                        Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                          Make_Function_Call (Loc,
                            Name                   =>
                              New_Occurrence_Of (RTE (RE_Base_Address), Loc),
                            Parameter_Associations => New_List (
                              Unchecked_Convert_To (RTE (RE_Address),
                                Duplicate_Subexpr (Prefix (Exp)))))));

               --  Similar case to the previous one but the expression is a
               --  renaming of an explicit dereference.

               elsif Nkind (Exp) = N_Identifier
                 and then Present (Renamed_Object (Entity (Exp)))
                 and then Nkind (Renamed_Object (Entity (Exp)))
                            = N_Explicit_Dereference
               then
                  Tag_Node :=
                    Make_Explicit_Dereference (Loc,
                      Prefix =>
                        Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                          Make_Function_Call (Loc,
                            Name                   =>
                              New_Occurrence_Of (RTE (RE_Base_Address), Loc),
                            Parameter_Associations => New_List (
                              Unchecked_Convert_To (RTE (RE_Address),
                                Duplicate_Subexpr
                                  (Prefix
                                    (Renamed_Object (Entity (Exp)))))))));

               --  Common case: obtain the address of the actual object and
               --  displace the pointer to reference the base of the object.

               else
                  Tag_Node :=
                    Make_Explicit_Dereference (Loc,
                      Prefix =>
                        Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                          Make_Function_Call (Loc,
                            Name               =>
                              New_Occurrence_Of (RTE (RE_Base_Address), Loc),
                            Parameter_Associations => New_List (
                              Make_Attribute_Reference (Loc,
                                Prefix         => Duplicate_Subexpr (Exp),
                                Attribute_Name => Name_Address)))));
               end if;
            else
               Tag_Node :=
                 Make_Attribute_Reference (Loc,
                   Prefix         => Duplicate_Subexpr (Exp),
                   Attribute_Name => Name_Tag);
            end if;

            --  Suppress junk access chacks on RE_Tag_Ptr

            Insert_Action (Exp,
              Make_Raise_Program_Error (Loc,
                Condition =>
                  Make_Op_Gt (Loc,
                    Left_Opnd  => Build_Get_Access_Level (Loc, Tag_Node),
                    Right_Opnd => Dynamic_Subprogram_Access_Level (Func)),
                Reason    => PE_Accessibility_Check_Failed),
              Suppress => Access_Check);
         end;
      end if;
   end Apply_Accessibility_Check_For_Class_Wide_Return;

   -----------------------------------------------------------
   -- Apply_Accessibility_Check_For_Discriminated_Allocator --
   -----------------------------------------------------------

   procedure Apply_Accessibility_Check_For_Discriminated_Allocator
     (N : Node_Id)
   is
      Loc     : constant Source_Ptr := Sloc (N);
      Typ     : constant Entity_Id  := Etype (Expression (N));
      Exp     : constant Node_Id    := Unqualify (Expression (N));
      Exp_Typ : constant Entity_Id  := Etype (Exp);

      Discr      : Entity_Id;
      Discr_Elmt : Elmt_Id;
      Discr_Exp  : Node_Id;

   --  Start of Apply_Accessibility_Check_For_Discriminated_Allocator

   begin
      --  Return immediately if accessibility checks are suppressed for N

      if Scope_Suppress.Suppress (Accessibility_Check)
        or else No_Dynamic_Accessibility_Checks_Enabled (N)
      then
         return;

      --  Check that the allocator's access discriminants do not designate
      --  entities that the allocator could outlive (RM 4.8(10.1)).

      elsif not Has_Anonymous_Access_Discriminant (Typ) then
         return;

      --  We ignore coextensions as they cannot be implemented under the
      --  "small integer" model.

      elsif Is_Static_Coextension (N) or else Is_Dynamic_Coextension (N) then
         return;

      --  If we are allocating a function result, then that function will
      --  perform the check.

      elsif Nkind (Exp) = N_Function_Call then
         return;

      --  When the value of the access discriminant is determined by
      --  an object with an unconstrained nominal subtype, its level
      --  is the accessibility level of the object (RM 3.10.2(12.4)).

      elsif not Is_Constrained (Exp_Typ) then
         Insert_Action (N,
           Make_Raise_Program_Error (Loc,
             Condition =>
               Make_Op_Gt (Loc,
                 Left_Opnd  => Dynamic_Accessibility_Level (Exp),
                 Right_Opnd => Dynamic_Accessibility_Level (N)),
             Reason    => PE_Accessibility_Check_Failed),
           Suppress => Access_Check);

         return;
      end if;

      Discr := First_Discriminant (Typ);
      Discr_Elmt := First_Elmt (Discriminant_Constraint (Exp_Typ));

      while Present (Discr) loop
         if Is_Anonymous_Access_Type (Etype (Discr)) then
            Discr_Exp := Node (Discr_Elmt);

            --  The accessibility level of a function call whose result type is
            --  an anonymous access type used to define the discriminant of an
            --  object is the level of the object (RM 3.10.2(10.3, 14.1)).

            if Nkind (Original_Node (Discr_Exp)) = N_Function_Call
              and then
                Is_Anonymous_Access_Type (Etype (Original_Node (Discr_Exp)))
            then
               null;

            else
               Insert_Action (N,
                 Make_Raise_Program_Error (Loc,
                   Condition =>
                     Make_Op_Gt (Loc,
                       Left_Opnd  => Dynamic_Accessibility_Level (Discr_Exp),
                       Right_Opnd => Dynamic_Accessibility_Level (N)),
                   Reason    => PE_Accessibility_Check_Failed),
                 Suppress => Access_Check);
            end if;
         end if;

         Next_Discriminant (Discr);
         Next_Elmt (Discr_Elmt);
      end loop;
   end Apply_Accessibility_Check_For_Discriminated_Allocator;

   --------------------------------------------------------
   -- Apply_Accessibility_Check_For_Discriminated_Return --
   --------------------------------------------------------

   --  A case that is not addressed today is the case where we need to check
   --  an access discriminant subcomponent of the function result other than
   --  a discriminant of the function result. This case can only happen if the
   --  function result type has an unconstrained subcomponent subtype that has
   --  an access discriminant (which implies that the function result type must
   --  be limited).

   --  A further corner case of that corner case arises if the limited result
   --  type is class-wide and it is not known statically whether this access
   --  discriminant subcomponent exists. The easiest way to address it properly
   --  would probably involve adding a compiler-generated dispatching procedure
   --  and a dispatching call could be used to perform the check in a context
   --  where we know statically the specific type of the function result.
   --  Finding a less important unimplemented case would be challenging.

   procedure Apply_Accessibility_Check_For_Discriminated_Return
     (Exp : Node_Id; Func : Entity_Id)
   is
      function Constraint_Bearing_Subtype_If_Any
        (Exp : Node_Id) return Node_Id;
      --  If we can locate a constrained subtype whose constraint applies
      --  to Exp, then return that. Otherwise, return Etype (Exp).

      ---------------------------------------
      -- Constraint_Bearing_Subtype_If_Any --
      ---------------------------------------

      function Constraint_Bearing_Subtype_If_Any
        (Exp : Node_Id) return Entity_Id
      is
         Result : Entity_Id := Etype (Exp);

      begin
         if Is_Constrained (Result) then
            return Result;
         end if;

         --  Look through expansion-generated levels of indirection
         --  to find a constrained subtype. Yuck. This comes up in
         --  some cases when the unexpanded source returns an aggregate.

         if Nkind (Exp) = N_Explicit_Dereference then
            declare
               P : Node_Id := Prefix (Exp);

            begin
               while Is_Entity_Name (P)
                 and then Ekind (Entity (P)) = E_Constant
                 and then Present (Constant_Value (Entity (P)))
               loop
                  P := Constant_Value (Entity (P));
               end loop;

               if Nkind (P) = N_Allocator
                 and then Nkind (Expression (P)) = N_Qualified_Expression
               then
                  Result := Etype (Expression (P));
               end if;
            end;
         end if;

         if Is_Constrained (Result) then
            return Result;
         end if;

         --  No constrained subtype found

         return Etype (Exp);
      end Constraint_Bearing_Subtype_If_Any;

      --  Local variables

      Discr      : Entity_Id;
      Discr_Elmt : Elmt_Id;
      Exp_Typ    : Entity_Id;

   --  Start of Apply_Accessibility_Check_For_Discriminated_Return

   begin
      --  ??? Do not generate a check if version is Ada 95 (or earlier).
      --  It is unclear whether this is really correct, or is just a stopgap
      --  measure. Investigation is needed to decide how post-Ada-95 binding
      --  interpretation changes in RM 3.10.2 should interact with Ada 95's
      --  return-by-reference model for functions with limited result types
      --  (which was abandoned in Ada 2005).

      if Ada_Version <= Ada_95 then
         return;

      --  If we are returning a function result, then that function will
      --  perform the check.

      elsif Nkind (Unqualify (Exp)) = N_Function_Call then
         return;

     --  ??? Cope with the consequences of the Disable_Tagged_Cases flag
     --  in accessibility.adb (which can cause the extra formal parameter
     --  needed for the check(s) generated here to be missing in the case
     --  of a tagged result type); this is a workaround and can
     --  prevent generation of a required check (or even a required
     --  legality check - see "statically too deep" check below).

      elsif No (Extra_Accessibility_Of_Result (Func)) then
         return;
      end if;

      Exp_Typ := Constraint_Bearing_Subtype_If_Any (Exp);

      --  When the value of the access discriminant is determined by
      --  an object with an unconstrained nominal subtype, its level
      --  is the accessibility level of the object (RM 3.10.2(12.4)).

      if not Is_Constrained (Exp_Typ) then
         Apply_Accessibility_Check_For_Anonymous_Return (Exp, Func, Exp);

         return;
      end if;

      Discr := First_Discriminant (Etype (Func));
      Discr_Elmt := First_Elmt (Discriminant_Constraint (Exp_Typ));

      while Present (Discr) loop
         if Is_Anonymous_Access_Type (Etype (Discr)) then
            Apply_Accessibility_Check_For_Anonymous_Return
              (Node (Discr_Elmt), Func, Exp);
         end if;

         Next_Discriminant (Discr);
         Next_Elmt (Discr_Elmt);
      end loop;
   end Apply_Accessibility_Check_For_Discriminated_Return;

   ----------------------------------------------
   -- Apply_Accessibility_Check_For_Conversion --
   ----------------------------------------------

   procedure Apply_Accessibility_Check_For_Conversion
     (N           : Node_Id;
      Typ         : Entity_Id;
      Insert_Node : Node_Id)
   is
      Loc : constant Source_Ptr := Sloc (N);

      Check_Cond  : Node_Id;
      Param_Ent   : Entity_Id;
      Param_Level : Node_Id;
      Type_Level  : Node_Id;

   begin
      if Inside_A_Generic then
         return;
      end if;

      --  Verify we haven't tried to add a dynamic accessibility check when we
      --  shouldn't.

      pragma Assert (not No_Dynamic_Accessibility_Checks_Enabled (N));

      Param_Ent := Param_Entity (N);

      --  Stand-alone object of an anonymous access type (SAOOAAAT)

      if Ada_Version >= Ada_2012
         and then No (Param_Ent)
         and then Is_Entity_Name (N)
         and then Ekind (Entity (N)) in E_Constant | E_Variable
         and then Present (Extra_Accessibility (Entity (N)))
      then
         Param_Ent := Entity (N);
      end if;

      --  Apply the run-time check only if the access object has an associated
      --  extra access level object and when accessibility checks are enabled.

      if Present (Param_Ent)
        and then Present (Extra_Accessibility (Param_Ent))
        and then not Accessibility_Checks_Suppressed (Param_Ent)
        and then not Accessibility_Checks_Suppressed (Typ)
      then
         --  Obtain the parameter's accessibility level

         Param_Level :=
           New_Occurrence_Of (Extra_Accessibility (Param_Ent), Loc);

         --  Use the dynamic accessibility parameter for the function's result
         --  when one has been created instead of statically referring to the
         --  deepest type level so as to appropriatly handle the rules for
         --  RM 3.10.2 (10.1/3).

         if Ekind (Scope (Param_Ent)) = E_Function
           and then In_Return_Value (N)
           and then Ekind (Typ) = E_Anonymous_Access_Type
         then
            --  Associate the level of the result type to the extra result
            --  accessibility parameter belonging to the current function.

            if Present (Extra_Accessibility_Of_Result (Scope (Param_Ent))) then
               Type_Level :=
                 New_Occurrence_Of
                   (Extra_Accessibility_Of_Result (Scope (Param_Ent)), Loc);

            --  In Ada 2005 and earlier modes, a result extra accessibility
            --  parameter is not generated and no dynamic check is performed.

            else
               return;
            end if;

         --  Otherwise get the type's accessibility level normally

         else
            Type_Level := Dynamic_Type_Access_Level (Typ, Deepest => True);
         end if;

         --  Raise Program_Error if the accessibility level of the access
         --  parameter is deeper than the level of the target access type.

         Check_Cond :=
           Make_Op_Gt (Loc,
             Left_Opnd  => Param_Level,
             Right_Opnd => Type_Level);

         Insert_Action (Insert_Node,
           Make_Raise_Program_Error (Loc,
             Condition => Check_Cond,
             Reason    => PE_Accessibility_Check_Failed));

         Analyze_And_Resolve (N);

         --  If constant folding has happened on the condition for the
         --  generated error, then warn about it being unconditional.

         if Nkind (Check_Cond) = N_Identifier
           and then Entity (Check_Cond) = Standard_True
         then
            Error_Msg_Warn := SPARK_Mode /= On;
            Error_Msg_N ("accessibility check fails<<", N);
            Error_Msg_N ("\Program_Error [<<", N);
         end if;
      end if;
   end Apply_Accessibility_Check_For_Conversion;

   ------------------------------------------
   -- Apply_Accessibility_Check_For_Return --
   ------------------------------------------

   procedure Apply_Accessibility_Check_For_Return
     (Exp  : Node_Id;
      Func : Entity_Id)
   is
      Typ : constant Entity_Id := Etype (Func);

   begin
      --  Return immediately if accessibility checks are suppressed for Func

      if Accessibility_Checks_Suppressed (Func) then
         return;
      end if;

      --  No checks are needed for the return synthesized in a function whose
      --  body has been wrapped in a nested _Wrapped_Statements function.

      if Present (Wrapped_Statements (Func)) then
         return;
      end if;

      --  Ada 2005 (AI95-344): If the result type is class-wide, then insert
      --  a check that the level of the return expression's underlying type
      --  is not deeper than the level of the master enclosing the function.

      if Is_Class_Wide_Type (Typ) then
         Apply_Accessibility_Check_For_Class_Wide_Return (Exp, Func);

      --  Check that the access result does not designate an entity that
      --  the result could outlive.

      elsif Ekind (Typ) = E_Anonymous_Access_Type then
         Apply_Accessibility_Check_For_Anonymous_Return (Exp, Func, Exp);

      --  Check that the result's access discriminants do not designate
      --  entities that the result could outlive (RM 6.5(21)).

      elsif Has_Anonymous_Access_Discriminant (Typ) then
         Apply_Accessibility_Check_For_Discriminated_Return (Exp, Func);
      end if;
   end Apply_Accessibility_Check_For_Return;

   ------------------------------------------
   -- Check_Return_Construct_Accessibility --
   ------------------------------------------

   procedure Check_Return_Construct_Accessibility
     (Return_Stmt : Node_Id;
      Stm_Entity  : Entity_Id)
   is
      Scope_Id : constant Entity_Id := Return_Applies_To (Stm_Entity);
      R_Type   : constant Entity_Id := Etype (Scope_Id);

      procedure Accessibility_Error;
      --  Give an error about the accessibility level of the return

      function First_Selector (Assoc : Node_Id) return Node_Id;
      --  Obtain the first selector or choice from a given association

      -------------------------
      -- Accessibility_Error --
      -------------------------

      procedure Accessibility_Error is
         Suffix : constant String :=
           (if Is_Expression_Function_Or_Completion (Scope_Id)
            then "in expression function"
            else "in return");
         Message : constant String :=
           "level of type of access discriminant is too deep " & Suffix;

      begin
         --  In an instance, this is a runtime check, but one we know will fail
         --  so generate an appropriate warning. As usual, this kind of warning
         --  is an error in SPARK mode or if No_Dynamic_Accessibility_Checks.

         if In_Instance_Body then
            Error_Msg_Warn := SPARK_Mode /= On
                                and then not
                                  No_Dynamic_Accessibility_Checks_Enabled
                                                                 (Return_Stmt);
            Error_Msg_N (Message & "<<", Return_Stmt);
            Error_Msg_F ("\Program_Error [<<", Return_Stmt);
            Rewrite (Return_Stmt,
              Make_Raise_Program_Error (Sloc (Return_Stmt),
                Reason => PE_Accessibility_Check_Failed));
            Set_Etype (Return_Stmt, Standard_Void_Type);

         else
            Error_Msg_N (Message, Return_Stmt);
         end if;
      end Accessibility_Error;

      --------------------
      -- First_Selector --
      --------------------

      function First_Selector (Assoc : Node_Id) return Node_Id is
      begin
         if Nkind (Assoc) = N_Component_Association then
            return First (Choices (Assoc));

         elsif Nkind (Assoc) = N_Discriminant_Association then
            return First (Selector_Names (Assoc));

         else
            raise Program_Error;
         end if;
      end First_Selector;

      --  Local declarations

      Assoc         : Node_Or_Entity_Id;
      Assoc_Expr    : Node_Id;
      Assoc_Present : Boolean := False;

      Unseen_Disc_Count : Nat := 0;
      Seen_Discs        : Elist_Id;
      Disc              : Entity_Id;
      First_Disc        : Entity_Id;

      Obj_Decl   : Node_Id;
      Return_Con : Node_Id;
      Unqual     : Node_Id;

   --  Start of processing for Check_Return_Construct_Accessibility

   begin
      pragma Assert (Nkind (Return_Stmt) in N_Extended_Return_Statement
                                          | N_Simple_Return_Statement);

      --  Only perform checks on record types with access discriminants and
      --  functions present in the source, but no checks are needed for the
      --  return synthesized in a function whose body has been wrapped in a
      --  nested _Wrapped_Statements function.

      if not Is_Record_Type (R_Type)
        or else not Has_Anonymous_Access_Discriminant (R_Type)
        or else not Comes_From_Source (Scope_Id)
        or else Present (Wrapped_Statements (Scope_Id))
      then
         return;
      end if;

      --  Fetch the object from the original return statement, in the case of a
      --  simple return statement the expression is part of the node.

      if Nkind (Original_Node (Return_Stmt)) = N_Extended_Return_Statement then
         --  Obtain the object definition from the expanded extended return

         Return_Con :=
           First (Return_Object_Declarations (Original_Node (Return_Stmt)));
         while Present (Return_Con) loop
            --  Inspect the original node to avoid object declarations
            --  expanded into renamings.

            if Nkind (Original_Node (Return_Con)) = N_Object_Declaration
              and then
                (Comes_From_Source (Original_Node (Return_Con))
                  or else
                    Is_Return_Object
                      (Defining_Identifier (Original_Node (Return_Con))))
            then
               exit;
            end if;

            Next (Return_Con);
         end loop;

         pragma Assert (Present (Return_Con));

         Return_Con := Original_Node (Return_Con);

      else
         Return_Con := Expression (Original_Node (Return_Stmt));
      end if;

      --  Obtain the accessibility levels of the expressions associated
      --  with anonymous access discriminants and apply a static check.

      --  Note the repeated use of Original_Node to avoid checking
      --  expanded code.

      Unqual := Original_Node (Unqualify (Original_Node (Return_Con)));

      --  Get the corresponding declaration based on the return object's
      --  identifier.

      if Is_Entity_Name (Unqual)
        and then
          Nkind (Parent (Entity (Unqual))) in N_Object_Declaration
                                            | N_Object_Renaming_Declaration
      then
         Obj_Decl := Original_Node (Parent (Entity (Unqual)));

      --  We were passed the object declaration directly, so use it

      elsif Nkind (Unqual) in N_Object_Declaration
                            | N_Object_Renaming_Declaration
      then
         Obj_Decl := Unqual;

      --  Otherwise, we are looking at something else

      else
         Obj_Decl := Empty;
      end if;

      --  Hop up object renamings when present

      if Present (Obj_Decl) then
         while Nkind (Obj_Decl) = N_Object_Renaming_Declaration loop
            if not Is_Entity_Name (Name (Obj_Decl)) then
               --  We may be looking at the expansion of iterators or
               --  some other internally generated construct, so it is safe
               --  to ignore checks ???

               if not Comes_From_Source (Obj_Decl) then
                  return;
               end if;

               Obj_Decl := Original_Node
                             (Declaration_Node
                               (Ultimate_Prefix (Name (Obj_Decl))));

            --  Move up to the next declaration based on the object's name

            else
               Obj_Decl := Original_Node
                             (Declaration_Node (Entity (Name (Obj_Decl))));
            end if;
         end loop;
      end if;

      --  Obtain the discriminant values from the return aggregate

      --  Do we cover extension aggregates correctly ???

      if Nkind (Unqual) = N_Aggregate then
         if Present (Expressions (Unqual)) then
            Assoc := First (Expressions (Unqual));
         else
            Assoc := First (Component_Associations (Unqual));
         end if;

      --  Apply the RM 3.10.2(12.4) rule to formal parameters

      elsif Is_Entity_Name (Unqual) and then Is_Formal (Entity (Unqual)) then
         if Static_Accessibility_Level (Unqual, In_Return_Context => True)
              > Static_Subprogram_Access_Level (Scope_Id)
         then
            Accessibility_Error;
         end if;

         return;

      --  There is an object declaration for the return object

      elsif Present (Obj_Decl) then
         --  When a subtype indication is present in an object declaration
         --  it must contain the object's discriminants.

         if Nkind (Object_Definition (Obj_Decl)) = N_Subtype_Indication then
            Assoc := First
                       (Constraints
                         (Constraint
                           (Object_Definition (Obj_Decl))));

         --  The object declaration contains an aggregate

         elsif Present (Expression (Obj_Decl)) then

            if Nkind (Unqualify (Expression (Obj_Decl))) = N_Aggregate then
               --  Grab the first associated discriminant expresion

               if Present
                    (Expressions (Unqualify (Expression (Obj_Decl))))
               then
                  Assoc := First
                             (Expressions
                               (Unqualify (Expression (Obj_Decl))));
               else
                  Assoc := First
                             (Component_Associations
                               (Unqualify (Expression (Obj_Decl))));
               end if;

            --  Otherwise, this is something else

            else
               return;
            end if;

         --  There are no supplied discriminants in the object declaration,
         --  so get them from the type definition since they must be default
         --  initialized.

         --  Do we handle constrained subtypes correctly ???

         elsif Nkind (Unqual) = N_Object_Declaration then
            Assoc := First_Discriminant
                       (Etype (Object_Definition (Obj_Decl)));

         else
            Assoc := First_Discriminant (Etype (Unqual));
         end if;

      --  When we are not looking at an aggregate or an identifier, return
      --  since any other construct (like a function call) is not
      --  applicable since checks will be performed on the side of the
      --  callee.

      else
         return;
      end if;

      --  Obtain the discriminants so we know the actual type in case the
      --  value of their associated expression gets implicitly converted.

      if No (Obj_Decl) then
         pragma Assert (Nkind (Unqual) = N_Aggregate);

         Disc := First_Discriminant (Etype (Unqual));

      else
         Disc := First_Discriminant
                   (Etype (Defining_Identifier (Obj_Decl)));
      end if;

      --  Preserve the first discriminant for checking named associations

      First_Disc := Disc;

      --  Count the number of discriminants for processing an aggregate
      --  which includes an others.

      Disc := First_Disc;
      while Present (Disc) loop
         Unseen_Disc_Count := Unseen_Disc_Count + 1;

         Next_Discriminant (Disc);
      end loop;

      Seen_Discs := New_Elmt_List;

      --  Loop through each of the discriminants and check each expression
      --  associated with an anonymous access discriminant.

      --  When named associations occur in the return aggregate then
      --  discriminants can be in any order, so we need to ensure we do
      --  not continue to loop when all discriminants have been seen.

      Disc := First_Disc;
      while Present (Assoc)
        and then (Present (Disc) or else Assoc_Present)
        and then Unseen_Disc_Count > 0
      loop
         --  Handle named associations by searching through the names of
         --  the relevant discriminant components.

         if Nkind (Assoc)
              in N_Component_Association | N_Discriminant_Association
         then
            Assoc_Expr    := Expression (Assoc);
            Assoc_Present := True;

            --  We currently don't handle box initialized discriminants,
            --  however, since default initialized anonymous access
            --  discriminants are a corner case, this is ok for now ???

            if Nkind (Assoc) = N_Component_Association
              and then Box_Present (Assoc)
            then
               if Nkind (First_Selector (Assoc)) = N_Others_Choice then
                  Unseen_Disc_Count := 0;
               end if;

            --  When others is present we must identify a discriminant we
            --  haven't already seen so as to get the appropriate type for
            --  the static accessibility check.

            --  This works because all components within an others clause
            --  must have the same type.

            elsif Nkind (First_Selector (Assoc)) = N_Others_Choice then

               Disc := First_Disc;
               Outer : while Present (Disc) loop
                  declare
                     Current_Seen_Disc : Elmt_Id;
                  begin
                     --  Move through the list of identified discriminants

                     Current_Seen_Disc := First_Elmt (Seen_Discs);
                     while Present (Current_Seen_Disc) loop
                        --  Exit the loop when we found a match

                        exit when
                          Chars (Node (Current_Seen_Disc)) = Chars (Disc);

                        Next_Elmt (Current_Seen_Disc);
                     end loop;

                     --  When we have exited the above loop without finding
                     --  a match then we know that Disc has not been seen.

                     exit Outer when No (Current_Seen_Disc);
                  end;

                  Next_Discriminant (Disc);
               end loop Outer;

               --  If we got to an others clause with a non-zero
               --  discriminant count there must be a discriminant left to
               --  check.

               pragma Assert (Present (Disc));

               --  Set the unseen discriminant count to zero because we know
               --  an others clause sets all remaining components of an
               --  aggregate.

               Unseen_Disc_Count := 0;

            --  Move through each of the selectors in the named association
            --  and obtain a discriminant for accessibility checking if one
            --  is referenced in the list. Also track which discriminants
            --  are referenced for the purpose of handling an others clause.

            else
               declare
                  Assoc_Choice : Node_Id;
                  Curr_Disc    : Node_Id;
               begin

                  Disc      := Empty;
                  Curr_Disc := First_Disc;
                  while Present (Curr_Disc) loop
                     --  Check each of the choices in the associations for a
                     --  match to the name of the current discriminant.

                     Assoc_Choice := First_Selector (Assoc);
                     while Present (Assoc_Choice) loop
                        --  When the name matches we track that we have seen
                        --  the discriminant, but instead of exiting the
                        --  loop we continue iterating to make sure all the
                        --  discriminants within the named association get
                        --  tracked.

                        if Chars (Assoc_Choice) = Chars (Curr_Disc) then
                           Append_Elmt (Curr_Disc, Seen_Discs);

                           Disc              := Curr_Disc;
                           Unseen_Disc_Count := Unseen_Disc_Count - 1;
                        end if;

                        Next (Assoc_Choice);
                     end loop;

                     Next_Discriminant (Curr_Disc);
                  end loop;
               end;
            end if;

         --  Unwrap the associated expression if we are looking at a default
         --  initialized type declaration. In this case Assoc is not really
         --  an association, but a component declaration. Should Assoc be
         --  renamed in some way to be more clear ???

         --  This occurs when the return object does not initialize
         --  discriminant and instead relies on the type declaration for
         --  their supplied values.

         elsif Nkind (Assoc) in N_Entity
           and then Ekind (Assoc) = E_Discriminant
         then
            Append_Elmt (Disc, Seen_Discs);

            Assoc_Expr        := Discriminant_Default_Value (Assoc);
            Unseen_Disc_Count := Unseen_Disc_Count - 1;

         --  Otherwise, there is nothing to do because Assoc is an
         --  expression within the return aggregate itself.

         else
            Append_Elmt (Disc, Seen_Discs);

            Assoc_Expr        := Assoc;
            Unseen_Disc_Count := Unseen_Disc_Count - 1;
         end if;

         --  Check the static accessibility level of the expression when the
         --  discriminant is of an anonymous access type.

         if Present (Assoc_Expr)
           and then Present (Disc)
           and then Ekind (Etype (Disc)) = E_Anonymous_Access_Type
           and then
             Static_Accessibility_Level
               (Assoc_Expr, In_Return_Context => True)
                 > Static_Subprogram_Access_Level (Scope_Id)
         then
            Accessibility_Error;
         end if;

         --  Iterate over the discriminants, except when we have encountered
         --  a named association since the discriminant order becomes
         --  irrelevant in that case.

         if not Assoc_Present then
            Next_Discriminant (Disc);
         end if;

         --  Iterate over associations

         if not Is_List_Member (Assoc) then
            exit;
         else
            Next (Assoc);
         end if;
      end loop;
   end Check_Return_Construct_Accessibility;

   ---------------------------------
   -- Dynamic_Accessibility_Level --
   ---------------------------------

   function Dynamic_Accessibility_Level
     (Expr              : Node_Id;
      In_Return_Context : Boolean := False;
      Allow_Alt_Model   : Boolean := True) return Node_Id
   is
   begin
      return
        Accessibility_Level (Expr              => Expr,
                             Level             => Dynamic_Level,
                             Static            => False,
                             In_Return_Context => In_Return_Context,
                             Allow_Alt_Model   => Allow_Alt_Model).N;
   end Dynamic_Accessibility_Level;

   --------------------------------
   -- Dynamic_Local_Access_Level --
   --------------------------------

   function Dynamic_Local_Access_Level (E : Entity_Id) return Node_Id is
      Loc          : constant Source_Ptr := Sloc (E);
      Static_Level : constant Uint       := Static_Local_Access_Level (E);
      Subp         : constant Entity_Id  := Enclosing_Subprogram (E);

   begin
      if Present (Subp) then
         return Offset_Static_Level (Static_Level, Subp);
      else
         return Make_Integer_Literal (Loc, Static_Level);
      end if;
   end Dynamic_Local_Access_Level;

   -------------------------------------
   -- Dynamic_Subprogram_Access_Level --
   -------------------------------------

   function Dynamic_Subprogram_Access_Level (Subp : Entity_Id) return Node_Id
   is
   begin
      if Present (Alias (Subp)) then
         return Dynamic_Subprogram_Access_Level (Alias (Subp));
      else
         return Dynamic_Local_Access_Level (Subp);
      end if;
   end Dynamic_Subprogram_Access_Level;

   -------------------------------
   -- Dynamic_Type_Access_Level --
   -------------------------------

   function Dynamic_Type_Access_Level
     (Typ             : Entity_Id;
      Deepest         : Boolean := False;
      Allow_Alt_Model : Boolean := True) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Typ);
      Val : constant Type_Level_T :=
              Type_Access_Level (Typ, Deepest, Allow_Alt_Model, Empty);

   begin
      if Present (Val.Base) then
         declare
            Dynamic_Level : constant Node_Id :=
              Dynamic_Local_Access_Level (Val.Base);

         begin
            if Val.Offset /= Uint_0 then
               return
                 Make_Op_Add (Loc,
                   Left_Opnd  => Dynamic_Level,
                   Right_Opnd => Make_Integer_Literal (Loc, Val.Offset));
            else
               return Dynamic_Level;
            end if;
         end;

      else
         return Make_Integer_Literal (Loc, Val.Offset);
      end if;
   end Dynamic_Type_Access_Level;

   -------------------------
   -- Extra_Accessibility --
   -------------------------

   function Extra_Accessibility (Id : Entity_Id) return Entity_Id is
   begin
      if Present (Renamed_Object (Id))
        and then Is_Entity_Name (Renamed_Object (Id))
      then
         return Extra_Accessibility (Entity (Renamed_Object (Id)));
      end if;

      if Is_Formal (Id) or else Ekind (Id) in E_Constant | E_Variable then
         return Extra_Accessibility_Of_Object (Id);
      else
         return Empty;
      end if;
   end Extra_Accessibility;

   -----------------------
   -- Has_Access_Values --
   -----------------------

   function Has_Access_Values (T : Entity_Id) return Boolean
   is
      Typ : constant Entity_Id := Underlying_Type (T);

   begin
      --  Case of a private type which is not completed yet. This can only
      --  happen in the case of a generic formal type appearing directly, or
      --  as a component of the type to which this function is being applied
      --  at the top level. Return False in this case, since we certainly do
      --  not know that the type contains access types.

      if No (Typ) then
         return False;

      elsif Is_Access_Type (Typ) then
         return True;

      elsif Is_Array_Type (Typ) then
         return Has_Access_Values (Component_Type (Typ));

      elsif Is_Record_Type (Typ) then
         declare
            Comp : Entity_Id;

         begin
            --  Loop to check components

            Comp := First_Component_Or_Discriminant (Typ);
            while Present (Comp) loop

               --  Check for access component, tag field does not count, even
               --  though it is implemented internally using an access type.

               if Has_Access_Values (Etype (Comp))
                 and then Chars (Comp) /= Name_uTag
               then
                  return True;
               end if;

               Next_Component_Or_Discriminant (Comp);
            end loop;
         end;

         return False;

      else
         return False;
      end if;
   end Has_Access_Values;

   ---------------------------------------
   -- Has_Anonymous_Access_Discriminant --
   ---------------------------------------

   function Has_Anonymous_Access_Discriminant (Typ : Entity_Id) return Boolean
   is
      Disc : Node_Id;

   begin
      if not Has_Discriminants (Typ) then
         return False;
      end if;

      Disc := First_Discriminant (Typ);
      while Present (Disc) loop
         if Ekind (Etype (Disc)) = E_Anonymous_Access_Type then
            return True;
         end if;

         Next_Discriminant (Disc);
      end loop;

      return False;
   end Has_Anonymous_Access_Discriminant;

   --------------------------------------------
   -- Has_Unconstrained_Access_Discriminants --
   --------------------------------------------

   function Has_Unconstrained_Access_Discriminants
     (Subtyp : Entity_Id) return Boolean
   is
      Discr : Entity_Id;

   begin
      if Has_Discriminants (Subtyp)
        and then not Is_Constrained (Subtyp)
      then
         Discr := First_Discriminant (Subtyp);
         while Present (Discr) loop
            if Ekind (Etype (Discr)) = E_Anonymous_Access_Type then
               return True;
            end if;

            Next_Discriminant (Discr);
         end loop;
      end if;

      return False;
   end Has_Unconstrained_Access_Discriminants;

   --------------------------------------
   -- Needs_Result_Accessibility_Level --
   --------------------------------------

   function Needs_Result_Accessibility_Level
     (Func_Id : Entity_Id) return Boolean
   is
      Func_Typ : constant Entity_Id := Underlying_Type (Etype (Func_Id));

      function Has_Unconstrained_Access_Discriminant_Component
        (Comp_Typ : Entity_Id) return Boolean;
      --  Returns True if any component of the type has an unconstrained access
      --  discriminant.

      -----------------------------------------------------
      -- Has_Unconstrained_Access_Discriminant_Component --
      -----------------------------------------------------

      function Has_Unconstrained_Access_Discriminant_Component
        (Comp_Typ :  Entity_Id) return Boolean
      is
      begin
         if not Is_Limited_Type (Comp_Typ) then
            return False;

         --  Only limited types can have access discriminants with
         --  defaults.

         elsif Has_Unconstrained_Access_Discriminants (Comp_Typ) then
            return True;

         elsif Is_Array_Type (Comp_Typ) then
            return Has_Unconstrained_Access_Discriminant_Component
                     (Underlying_Type (Component_Type (Comp_Typ)));

         elsif Is_Record_Type (Comp_Typ) then
            declare
               Comp : Entity_Id;

            begin
               Comp := First_Component (Comp_Typ);
               while Present (Comp) loop
                  if Has_Unconstrained_Access_Discriminant_Component
                       (Underlying_Type (Etype (Comp)))
                  then
                     return True;
                  end if;

                  Next_Component (Comp);
               end loop;
            end;
         end if;

         return False;
      end Has_Unconstrained_Access_Discriminant_Component;

      Disable_Tagged_Cases : constant Boolean := True;
      --  Flag used to temporarily disable a "True" result for tagged types.
      --  See comments further below for details.

   --  Start of processing for Needs_Result_Accessibility_Level

   begin
      --  False if completion unavailable, which can happen when we are
      --  analyzing an abstract subprogram or if the subprogram has
      --  delayed freezing.

      if No (Func_Typ) then
         return False;

      --  False if not a function, also handle enum-lit renames case

      elsif Func_Typ = Standard_Void_Type
        or else Is_Scalar_Type (Func_Typ)
      then
         return False;

      --  Handle a corner case, a cross-dialect subp renaming. For example,
      --  an Ada 2012 renaming of an Ada 2005 subprogram. This can occur when
      --  an Ada 2005 (or earlier) unit references predefined run-time units.

      elsif Present (Alias (Func_Id)) then

         --  Unimplemented: a cross-dialect subp renaming which does not set
         --  the Alias attribute (e.g., a rename of a dereference of an access
         --  to subprogram value). ???

         return Present (Extra_Accessibility_Of_Result (Alias (Func_Id)));

      --  Remaining cases require Ada 2012 mode, unless they are dispatching
      --  operations, since they may be overridden by Ada_2012 primitives.

      elsif Ada_Version < Ada_2012
        and then not Is_Dispatching_Operation (Func_Id)
      then
         return False;

      --  Handle the situation where a result is an anonymous access type
      --  RM 3.10.2 (10.3/3).

      elsif Ekind (Func_Typ) = E_Anonymous_Access_Type then
         return True;

      --  In the case of, say, a null tagged record result type, the need for
      --  this extra parameter might not be obvious so this function returns
      --  True for all tagged types for compatibility reasons.

      --  A function with, say, a tagged null controlling result type might
      --  be overridden by a primitive of an extension having an access
      --  discriminant and the overrider and overridden must have compatible
      --  calling conventions (including implicitly declared parameters).

      --  Similarly, values of one access-to-subprogram type might designate
      --  both a primitive subprogram of a given type and a function which is,
      --  for example, not a primitive subprogram of any type. Again, this
      --  requires calling convention compatibility. It might be possible to
      --  solve these issues by introducing wrappers, but that is not the
      --  approach that was chosen.

      --  Note: Despite the reasoning noted above, the extra accessibility
      --  parameter for tagged types is disabled for performance reasons.

      elsif Is_Tagged_Type (Func_Typ) then
         return not Disable_Tagged_Cases;

      elsif Has_Unconstrained_Access_Discriminants (Func_Typ) then
         return True;

      elsif Has_Unconstrained_Access_Discriminant_Component (Func_Typ) then
         return True;

      --  False for all other cases

      else
         return False;
      end if;
   end Needs_Result_Accessibility_Level;

   -------------------------
   -- Offset_Static_Level --
   -------------------------

   --  An alternate, more sophisticated formula could be:
   --
   --    if Extra_Accessibility (Subp) > Static_Accessibility (Subp)
   --    then Level + Extra_Accessibility (Subp) - Static_Accessibility (Subp)
   --    else Level
   --
   --  and would yield more progressive dynamic levels in call chains, but it
   --  would complicate the run-time computation and not really buy anything.

   function Offset_Static_Level
     (Level : Uint;
      Subp  : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Subp);

   begin
      pragma Assert (Is_Subprogram (Subp));

      if Present (Extra_Accessibility_Of_Subprogram (Subp)) then
         return
           Make_Op_Add (Loc,
             Left_Opnd  =>
               New_Occurrence_Of
                 (Extra_Accessibility_Of_Subprogram (Subp), Loc),
             Right_Opnd => Make_Integer_Literal (Loc, Level));

      else
         return Make_Integer_Literal (Loc, Level);
      end if;
   end Offset_Static_Level;

   ------------------------------------------
   -- Prefix_With_Safe_Accessibility_Level --
   ------------------------------------------

   function Prefix_With_Safe_Accessibility_Level
     (N   : Node_Id;
      Typ : Entity_Id) return Boolean
   is
      P        : constant Node_Id      := Prefix (N);
      Aname    : constant Name_Id      := Attribute_Name (N);
      Attr_Id  : constant Attribute_Id := Get_Attribute_Id (Aname);
      Btyp     : constant Entity_Id    := Base_Type (Typ);

      function Safe_Value_Conversions return Boolean;
      --  Return False if the prefix has a value conversion of an array type

      ----------------------------
      -- Safe_Value_Conversions --
      ----------------------------

      function Safe_Value_Conversions return Boolean is
         PP : Node_Id := P;

      begin
         loop
            if Nkind (PP) in N_Selected_Component | N_Indexed_Component then
               PP := Prefix (PP);

            elsif Comes_From_Source (PP)
              and then Nkind (PP) in N_Type_Conversion
                                   | N_Unchecked_Type_Conversion
              and then Is_Array_Type (Etype (PP))
            then
               return False;

            elsif Comes_From_Source (PP)
              and then Nkind (PP) = N_Qualified_Expression
              and then Is_Array_Type (Etype (PP))
              and then Nkind (Original_Node (Expression (PP))) in
                         N_Aggregate | N_Extension_Aggregate
            then
               return False;

            else
               exit;
            end if;
         end loop;

         return True;
      end Safe_Value_Conversions;

   --  Start of processing for Prefix_With_Safe_Accessibility_Level

   begin
      --  No check required for unchecked and unrestricted access

      if Attr_Id = Attribute_Unchecked_Access
        or else Attr_Id = Attribute_Unrestricted_Access
      then
         return True;

      --  Check value conversions

      elsif Ekind (Btyp) = E_General_Access_Type
        and then not Safe_Value_Conversions
      then
         return False;
      end if;

      return True;
   end Prefix_With_Safe_Accessibility_Level;

   --------------------------------
   -- Static_Accessibility_Level --
   --------------------------------

   function Static_Accessibility_Level
     (Expr              : Node_Id;
      Object_Decl_Level : Boolean := False;
      In_Return_Context : Boolean := False) return Uint
   is
      Level : constant Accessibility_Level_Kind :=
                (if Object_Decl_Level
                 then Accessibility.Object_Decl_Level
                 else Library_On_Dynamic_Level);

   begin
      return
        Accessibility_Level (Expr              => Expr,
                             Level             => Level,
                             Static            => True,
                             In_Return_Context => In_Return_Context).U;
   end Static_Accessibility_Level;

   -------------------------------
   -- Static_Local_Access_Level --
   -------------------------------

   function Static_Local_Access_Level (E : Entity_Id) return Uint is
   begin
      return Scope_Depth (Enclosing_Dynamic_Scope (E));
   end Static_Local_Access_Level;

   ------------------------------------
   -- Static_Subprogram_Access_Level --
   ------------------------------------

   function Static_Subprogram_Access_Level (Subp : Entity_Id) return Uint is
   begin
      if Present (Alias (Subp)) then
         return Static_Subprogram_Access_Level (Alias (Subp));
      else
         return Static_Local_Access_Level (Subp);
      end if;
   end Static_Subprogram_Access_Level;

   ------------------------------
   -- Static_Type_Access_Level --
   ------------------------------

   function Static_Type_Access_Level
     (Typ             : Entity_Id;
      Deepest         : Boolean := False;
      Allow_Alt_Model : Boolean := True;
      Assoc_Node      : Node_Id := Empty) return Uint
   is
      Val : constant Type_Level_T :=
              Type_Access_Level (Typ, Deepest, Allow_Alt_Model, Assoc_Node);

   begin
      if Present (Val.Base) then
         return Static_Local_Access_Level (Val.Base) + Val.Offset;
      else
         return Val.Offset;
      end if;
   end Static_Type_Access_Level;

   -----------------------
   -- Type_Access_Level --
   -----------------------

   function Type_Access_Level
     (Typ             : Entity_Id;
      Deepest         : Boolean := False;
      Allow_Alt_Model : Boolean := True;
      Assoc_Node      : Node_Id := Empty) return Type_Level_T
   is
      Btyp    : Entity_Id := Base_Type (Typ);
      Def_Ent : Entity_Id;

   begin
      if Ekind (Typ) = E_Anonymous_Access_Type
        and then not Is_Local_Anonymous_Access (Typ)
        and then Nkind (Associated_Node_For_Itype (Typ)) = N_Object_Declaration
        and then Deepest

         --  No_Dynamic_Accessibility_Checks override for alternative
         --  accessibility model.

        and then not (Allow_Alt_Model
                       and then No_Dynamic_Accessibility_Checks_Enabled (Typ))
      then
         --  Typ is the type of an Ada 2012 stand-alone object of an anonymous
         --  access type.

         return
           (Defining_Identifier (Associated_Node_For_Itype (Typ)), Uint_0);

      --  Ada 2005 (AI-230): For most cases of anonymous access types, we
      --  simply use the level where the type is declared. This is true for
      --  stand-alone object declarations, and for anonymous access types
      --  associated with components the level is the same as that of the
      --  enclosing composite type. However, special treatment is needed for
      --  the cases of access parameters, return objects of an anonymous access
      --  type, and, in Ada 95, access discriminants of limited types.

      elsif Is_Access_Type (Btyp) then
         if Ekind (Btyp) = E_Anonymous_Access_Type then
            --  No_Dynamic_Accessibility_Checks restriction override for
            --  alternative accessibility model.

            if Allow_Alt_Model
              and then No_Dynamic_Accessibility_Checks_Enabled (Btyp)
            then
               --  In the -gnatd_b model, the level of an anonymous access
               --  type is always that of the designated type.

               if Debug_Flag_Underscore_B then
                  return Type_Access_Level
                           (Designated_Type (Btyp), Allow_Alt_Model);
               end if;

               --  When an anonymous access type's Assoc_Node is specified,
               --  calculate the result based on the general accessibility
               --  level routine.

               --  We would like to use Associated_Node_For_Itype here instead,
               --  but in some cases it is not fine grained enough ???

               if Present (Assoc_Node) then
                  return
                    (Empty,
                     Static_Accessibility_Level
                       (Assoc_Node, Object_Decl_Level => True));
               end if;

               --  Otherwise take the context of the anonymous access type into
               --  account.

               --  Obtain the defining entity for the internally generated
               --  anonymous access type.

               Def_Ent := Defining_Entity_Or_Empty
                            (Associated_Node_For_Itype (Typ));

               if Present (Def_Ent) then
                  --  When the defining entity is a subprogram, then we know
                  --  that the anonymous access type Typ has been generated
                  --  for either an access formal or an access result.

                  --  Since we are only interested in the formal case, avoid
                  --  the anonymous access result type.

                  if Is_Subprogram (Def_Ent)
                    and then not (Ekind (Def_Ent) = E_Function
                                   and then Etype (Def_Ent) = Typ)
                  then
                     --  When the type comes from an anonymous access
                     --  parameter, the level is that of the parameter
                     --  in the called subprogram.

                     if In_Open_Scopes (Def_Ent) then
                        return (First_Formal (Def_Ent), Uint_0);

                     --  The level is the deepest possible in the caller

                     else
                        return (Empty, UI_From_Int (Int'Last));
                     end if;

                  --  When the type is an access discriminant, the level is
                  --  that of the type.

                  elsif Ekind (Def_Ent) = E_Discriminant then
                     return (Scope (Def_Ent), Uint_0);
                  end if;
               end if;

            --  If the type is a nonlocal anonymous access type (such as for
            --  an access parameter) we treat it as being declared at the
            --  library level to ensure that names such as X.all'access don't
            --  fail static accessibility checks.

            elsif not Is_Local_Anonymous_Access (Typ) then
               return (Empty, Scope_Depth (Standard_Standard));

            --  If this is a return object, the accessibility level is that of
            --  the result subtype of the enclosing function. The test here is
            --  little complicated, because we have to account for extended
            --  return statements that have been rewritten as blocks, in which
            --  case we have to find and the Is_Return_Object attribute of the
            --  itype's associated object. It would be nice to find a way to
            --  simplify this test, but it doesn't seem worthwhile to add a new
            --  flag just for purposes of this test. ???

            elsif Ekind (Scope (Btyp)) = E_Return_Statement
              or else
                (Is_Itype (Btyp)
                  and then Nkind (Associated_Node_For_Itype (Btyp)) =
                                                         N_Object_Declaration
                  and then
                    Is_Return_Object
                      (Defining_Identifier (Associated_Node_For_Itype (Btyp))))
            then
               return
                 Type_Access_Level
                   (Etype (Enclosing_Subprogram (Btyp)), Allow_Alt_Model);
            end if;
         end if;

         Btyp := Root_Type (Btyp);

         --  The accessibility level of anonymous access types associated with
         --  discriminants is that of the current instance of the type, and
         --  that's deeper than the type itself (AARM 3.10.2 (12.3.21)).

         --  AI-402: access discriminants have accessibility based on the
         --  object rather than the type in Ada 2005, so the above paragraph
         --  doesn't apply.

         --  ??? Needs completion with rules from AI-416

         if Ada_Version <= Ada_95
           and then Ekind (Typ) = E_Anonymous_Access_Type
           and then Present (Associated_Node_For_Itype (Typ))
           and then Nkind (Associated_Node_For_Itype (Typ)) =
                                                 N_Discriminant_Specification
         then
            return (Btyp, Uint_1);
         end if;
      end if;

      --  Return library level for a generic formal type. This is done because
      --  RM 3.10.2(19.4) says that "The statically deeper relationship does
      --  not apply to ... a descendant of a generic formal type". Rather than
      --  checking at each point where a static accessibility check is
      --  performed to see if we are dealing with a formal type, this rule is
      --  implemented by returning extreme values for a formal type depending
      --  on the Deepest parameter. More specifically, Deepest should be passed
      --  as True whenever the call occurs as part of a static accessibility
      --  check and the error case is the case where the type's level is too
      --  shallow (as opposed to too deep).

      if Is_Generic_Type (Root_Type (Btyp)) then
         if Deepest then
            return (Empty, UI_From_Int (Int'Last));
         else
            return (Empty, Scope_Depth (Standard_Standard));
         end if;
      end if;

      return (Btyp, Uint_0);
   end Type_Access_Level;

end Accessibility;
