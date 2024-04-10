------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        A C C E S S I B I L I T Y                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2022-2024, Free Software Foundation, Inc.         --
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
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Elists;         use Elists;
with Errout;         use Errout;
with Einfo.Utils;    use Einfo.Utils;
with Exp_Atag;       use Exp_Atag;
with Exp_Ch3;        use Exp_Ch3;
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
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;          use Stand;
with Tbuild;         use Tbuild;

package body Accessibility is

   ---------------------------
   -- Accessibility_Message --
   ---------------------------

   procedure Accessibility_Message (N : Node_Id; Typ : Entity_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      P     : constant Node_Id    := Prefix (N);
      Indic : Node_Id             := Parent (Parent (N));

   begin
      --  In an instance, this is a runtime check, but one we know will fail,
      --  so generate an appropriate warning.

      if In_Instance_Body then
         Error_Msg_Warn := SPARK_Mode /= On;
         Error_Msg_F
           ("non-local pointer cannot point to local object<<", P);
         Error_Msg_F ("\Program_Error [<<", P);
         Rewrite (N,
           Make_Raise_Program_Error (Loc,
             Reason => PE_Accessibility_Check_Failed));
         Set_Etype (N, Typ);
         return;

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
      In_Return_Context : Boolean := False;
      Allow_Alt_Model   : Boolean := True) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Expr);

      function Accessibility_Level (Expr : Node_Id) return Node_Id is
        (Accessibility_Level
          (Expr, Level, In_Return_Context, Allow_Alt_Model));
      --  Renaming of the enclosing function to facilitate recursive calls

      function Make_Level_Literal (Level : Uint) return Node_Id;
      --  Construct an integer literal representing an accessibility level with
      --  its type set to Natural.

      function Innermost_Master_Scope_Depth (N : Node_Id) return Uint;
      --  Returns the scope depth of the given node's innermost enclosing scope
      --  (effectively the accessibility level of the innermost enclosing
      --  master).

      function Function_Call_Or_Allocator_Level (N : Node_Id) return Node_Id;
      --  Centralized processing of subprogram calls which may appear in prefix
      --  notation.

      function Typ_Access_Level (Typ : Entity_Id) return Uint
        is (Type_Access_Level (Typ, Allow_Alt_Model));
      --  Renaming of Type_Access_Level with Allow_Alt_Model specified to avoid
      --  passing the parameter specifically in every call.

      ----------------------------------
      -- Innermost_Master_Scope_Depth --
      ----------------------------------

      function Innermost_Master_Scope_Depth (N : Node_Id) return Uint is
         Encl_Scop           : Entity_Id;
         Ent                 : Entity_Id;
         Node_Par            : Node_Id := Parent (N);
         Master_Lvl_Modifier : Int     := 0;

      begin
         --  Locate the nearest enclosing node (by traversing Parents)
         --  that Defining_Entity can be applied to, and return the
         --  depth of that entity's nearest enclosing scope.

         --  The RM 7.6.1(3) definition of "master" includes statements
         --  and conditions for loops among other things. Are these cases
         --  detected properly ???

         while Present (Node_Par) loop
            Ent := Defining_Entity_Or_Empty (Node_Par);

            if Present (Ent) then
               --  X'Old is nested within the current subprogram, so we do not
               --  want Find_Enclosing_Scope of that subprogram. If this is an
               --  allocator, then we're looking for the innermost master of
               --  the call, so again we do not want Find_Enclosing_Scope.

               if (Nkind (N) = N_Attribute_Reference
                    and then Attribute_Name (N) = Name_Old)
                 or else Nkind (N) = N_Allocator
               then
                  Encl_Scop := Ent;
               else
                  Encl_Scop := Find_Enclosing_Scope (Ent);
               end if;

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
                  --  simply return zero for the outermost scope.

                  if Scope_Depth_Set (Encl_Scop) then
                     return Scope_Depth (Encl_Scop) + Master_Lvl_Modifier;
                  else
                     return Uint_0;
                  end if;
               end if;

            --  For a return statement within a function, return
            --  the depth of the function itself. This is not just
            --  a small optimization, but matters when analyzing
            --  the expression in an expression function before
            --  the body is created.

            elsif Nkind (Node_Par) in N_Extended_Return_Statement
                                    | N_Simple_Return_Statement
            then
               return Scope_Depth (Enclosing_Subprogram (Node_Par));

            --  Statements are counted as masters

            elsif Is_Master (Node_Par) then
               Master_Lvl_Modifier := Master_Lvl_Modifier + 1;

            end if;

            Node_Par := Parent (Node_Par);
         end loop;

         --  Should never reach the following return

         pragma Assert (False);

         return Scope_Depth (Current_Scope) + 1;
      end Innermost_Master_Scope_Depth;

      ------------------------
      -- Make_Level_Literal --
      ------------------------

      function Make_Level_Literal (Level : Uint) return Node_Id is
         Result : constant Node_Id := Make_Integer_Literal (Loc, Level);

      begin
         Set_Etype (Result, Standard_Natural);
         return Result;
      end Make_Level_Literal;

      --------------------------------------
      -- Function_Call_Or_Allocator_Level --
      --------------------------------------

      function Function_Call_Or_Allocator_Level (N : Node_Id) return Node_Id is
         Par      : Node_Id;
         Prev_Par : Node_Id;
      begin
         --  Results of functions are objects, so we either get the
         --  accessibility of the function or, in case of a call which is
         --  indirect, the level of the access-to-subprogram type.

         --  This code looks wrong ???

         if Nkind (N) = N_Function_Call
           and then Ada_Version < Ada_2005
         then
            if Is_Entity_Name (Name (N)) then
               return Make_Level_Literal
                        (Subprogram_Access_Level (Entity (Name (N))));
            else
               return Make_Level_Literal
                        (Typ_Access_Level (Etype (Prefix (Name (N)))));
            end if;

         --  We ignore coextensions as they cannot be implemented under the
         --  "small-integer" model.

         elsif Nkind (N) = N_Allocator
           and then (Is_Static_Coextension (N)
                      or else Is_Dynamic_Coextension (N))
         then
            return Make_Level_Literal (Scope_Depth (Standard_Standard));
         end if;

         --  Named access types have a designated level

         if Is_Named_Access_Type (Etype (N)) then
            return Make_Level_Literal (Typ_Access_Level (Etype (N)));

         --  Otherwise, the level is dictated by RM 3.10.2 (10.7/3)

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
                  return Make_Level_Literal (Typ_Access_Level (Etype (N)));

               --  For function calls the level is that of the innermost
               --  master, otherwise (for allocators etc.) we get the level
               --  of the corresponding anonymous access type, which is
               --  calculated through the normal path of execution.

               elsif Nkind (N) = N_Function_Call then
                  return Make_Level_Literal
                           (Innermost_Master_Scope_Depth (Expr));
               end if;
            end if;

            if Nkind (N) = N_Function_Call then
               --  Dynamic checks are generated when we are within a return
               --  value or we are in a function call within an anonymous
               --  access discriminant constraint of a return object (signified
               --  by In_Return_Context) on the side of the callee.

               --  So, in this case, return accessibility level of the
               --  enclosing subprogram.

               if In_Return_Value (N)
                 or else In_Return_Context
               then
                  return Make_Level_Literal
                           (Subprogram_Access_Level (Current_Subprogram));
               end if;
            end if;

            --  When the call is being dereferenced the level is that of the
            --  enclosing master of the dereferenced call.

            if Nkind (Parent (N)) in N_Explicit_Dereference
                                   | N_Indexed_Component
                                   | N_Selected_Component
            then
               return Make_Level_Literal
                        (Innermost_Master_Scope_Depth (Expr));
            end if;

            --  Find any relevant enclosing parent nodes that designate an
            --  object being initialized.

            --  Note: The above is only relevant if the result is used "in its
            --  entirety" as RM 3.10.2 (10.2/3) states. However, this is
            --  accounted for in the case statement in the main body of
            --  Accessibility_Level for N_Selected_Component.

            Par      := Parent (Expr);
            Prev_Par := Empty;
            while Present (Par) loop
               --  Detect an expanded implicit conversion, typically this
               --  occurs on implicitly converted actuals in calls.

               --  Does this catch all implicit conversions ???

               if Nkind (Par) = N_Type_Conversion
                 and then Is_Named_Access_Type (Etype (Par))
               then
                  return Make_Level_Literal
                           (Typ_Access_Level (Etype (Par)));
               end if;

               --  Jump out when we hit an object declaration or the right-hand
               --  side of an assignment, or a construct such as an aggregate
               --  subtype indication which would be the result is not used
               --  "in its entirety."

               exit when Nkind (Par) in N_Object_Declaration
                           or else (Nkind (Par) = N_Assignment_Statement
                                     and then Name (Par) /= Prev_Par);

               Prev_Par := Par;
               Par      := Parent (Par);
            end loop;

            --  Assignment statements are handled in a similar way in
            --  accordance to the left-hand part. However, strictly speaking,
            --  this is illegal according to the RM, but this change is needed
            --  to pass an ACATS C-test and is useful in general ???

            case Nkind (Par) is
               when N_Object_Declaration =>
                  return Make_Level_Literal
                           (Scope_Depth
                             (Scope (Defining_Identifier (Par))));

               when N_Assignment_Statement =>
                  --  Return the accessibility level of the left-hand part

                  return Accessibility_Level
                           (Expr              => Name (Par),
                            Level             => Object_Decl_Level,
                            In_Return_Context => In_Return_Context);

               when others =>
                  return Make_Level_Literal
                           (Innermost_Master_Scope_Depth (Expr));
            end case;
         end if;
      end Function_Call_Or_Allocator_Level;

      --  Local variables

      E   : Node_Id := Original_Node (Expr);
      Pre : Node_Id;

   --  Start of processing for Accessibility_Level

   begin
      --  We could be looking at a reference to a formal due to the expansion
      --  of entries and other cases, so obtain the renaming if necessary.

      if Present (Param_Entity (Expr)) then
         E := Param_Entity (Expr);
      end if;

      --  Extract the entity

      if Nkind (E) in N_Has_Entity and then Present (Entity (E)) then
         E := Entity (E);

         --  Deal with a possible renaming of a private protected component

         if Ekind (E) in E_Constant | E_Variable and then Is_Prival (E) then
            E := Prival_Link (E);
         end if;
      end if;

      --  Perform the processing on the expression

      case Nkind (E) is
         --  The level of an aggregate is that of the innermost master that
         --  evaluates it as defined in RM 3.10.2 (10/4).

         when N_Aggregate =>
            return Make_Level_Literal (Innermost_Master_Scope_Depth (Expr));

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

            --  Unchecked or unrestricted attributes have unlimited depth

            elsif Attribute_Name (E) in Name_Address
                                      | Name_Unchecked_Access
                                      | Name_Unrestricted_Access
            then
               return Make_Level_Literal (Scope_Depth (Standard_Standard));

            --  'Access can be taken further against other special attributes,
            --  so handle these cases explicitly.

            elsif Attribute_Name (E)
                    in Name_Old | Name_Loop_Entry | Name_Result
            then
               --  Named access types

               if Is_Named_Access_Type (Etype (Pre)) then
                  return Make_Level_Literal
                           (Typ_Access_Level (Etype (Pre)));

               --  Anonymous access types

               elsif Nkind (Pre) in N_Has_Entity
                 and then Ekind (Entity (Pre)) not in Subprogram_Kind
                 and then Present (Get_Dynamic_Accessibility (Entity (Pre)))
                 and then Level = Dynamic_Level
               then
                  pragma Assert (Is_Anonymous_Access_Type (Etype (Pre)));
                  return New_Occurrence_Of
                           (Get_Dynamic_Accessibility (Entity (Pre)), Loc);

               --  Otherwise the level is treated in a similar way as
               --  aggregates according to RM 6.1.1 (35.1/4) which concerns
               --  an implicit constant declaration - in turn defining the
               --  accessibility level to be that of the implicit constant
               --  declaration.

               else
                  return Make_Level_Literal
                           (Innermost_Master_Scope_Depth (Expr));
               end if;

            else
               raise Program_Error;
            end if;

         --  This is the "base case" for accessibility level calculations which
         --  means we are near the end of our recursive traversal.

         when N_Defining_Identifier =>
            --  A dynamic check is performed on the side of the callee when we
            --  are within a return statement, so return a library-level
            --  accessibility level to null out checks on the side of the
            --  caller.

            if Is_Explicitly_Aliased (E)
              and then (In_Return_Context
                         or else (Level /= Dynamic_Level
                                   and then In_Return_Value (Expr)))
            then
               return Make_Level_Literal (Scope_Depth (Standard_Standard));

            --  Something went wrong and an extra accessibility formal has not
            --  been generated when one should have ???

            elsif Is_Formal (E)
              and then No (Get_Dynamic_Accessibility (E))
              and then Ekind (Etype (E)) = E_Anonymous_Access_Type
            then
               return Make_Level_Literal (Scope_Depth (Standard_Standard));

            --  Stand-alone object of an anonymous access type "SAOAAT"

            elsif (Is_Formal (E)
                    or else Ekind (E) in E_Variable
                                       | E_Constant)
              and then Present (Get_Dynamic_Accessibility (E))
              and then (Level = Dynamic_Level
                         or else Level = Zero_On_Dynamic_Level)
            then
               if Level = Zero_On_Dynamic_Level then
                  return Make_Level_Literal
                           (Scope_Depth (Standard_Standard));
               end if;

               --  No_Dynamic_Accessibility_Checks restriction override for
               --  alternative accessibility model.

               if Allow_Alt_Model
                 and then No_Dynamic_Accessibility_Checks_Enabled (E)
               then
                  --  In the alternative model the level is that of the
                  --  designated type entity's context.

                  if Debug_Flag_Underscore_B then
                     return Make_Level_Literal (Typ_Access_Level (Etype (E)));

                  --  Otherwise the level depends on the entity's context

                  elsif Is_Formal (E) then
                     return Make_Level_Literal
                              (Subprogram_Access_Level
                                (Enclosing_Subprogram (E)));
                  else
                     return Make_Level_Literal
                              (Scope_Depth (Enclosing_Dynamic_Scope (E)));
                  end if;
               end if;

               --  Return the dynamic level in the normal case

               return New_Occurrence_Of
                        (Get_Dynamic_Accessibility (E), Loc);

            --  Initialization procedures have a special extra accessibility
            --  parameter associated with the level at which the object
            --  being initialized exists

            elsif Ekind (E) = E_Record_Type
              and then Is_Limited_Record (E)
              and then Current_Scope = Init_Proc (E)
              and then Present (Init_Proc_Level_Formal (Current_Scope))
            then
               return New_Occurrence_Of
                        (Init_Proc_Level_Formal (Current_Scope), Loc);

            --  Current instance of the type is deeper than that of the type
            --  according to RM 3.10.2 (21).

            elsif Is_Type (E) then
               --  When restriction No_Dynamic_Accessibility_Checks is active
               --  along with -gnatd_b.

               if Allow_Alt_Model
                 and then No_Dynamic_Accessibility_Checks_Enabled (E)
                 and then Debug_Flag_Underscore_B
               then
                  return Make_Level_Literal (Typ_Access_Level (E));
               end if;

               --  Normal path

               return Make_Level_Literal (Typ_Access_Level (E) + 1);

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

            --  Named access types get their level from their associated type

            elsif Is_Named_Access_Type (Etype (E)) then
               return Make_Level_Literal
                        (Typ_Access_Level (Etype (E)));

            --  Check if E is an expansion-generated renaming of an iterator
            --  by examining Related_Expression. If so, determine the
            --  accessibility level based on the original expression.

            elsif Ekind (E) in E_Constant | E_Variable
              and then Present (Related_Expression (E))
            then
               return Accessibility_Level (Related_Expression (E));

            elsif Level = Dynamic_Level
               and then Ekind (E) in E_In_Parameter | E_In_Out_Parameter
               and then Present (Init_Proc_Level_Formal (Scope (E)))
            then
               return New_Occurrence_Of
                        (Init_Proc_Level_Formal (Scope (E)), Loc);

            --  Normal object - get the level of the enclosing scope

            else
               return Make_Level_Literal
                        (Scope_Depth (Enclosing_Dynamic_Scope (E)));
            end if;

         --  Handle indexed and selected components including the special cases
         --  whereby there is an implicit dereference, a component of a
         --  composite type, or a function call in prefix notation.

         --  We don't handle function calls in prefix notation correctly ???

         when N_Indexed_Component | N_Selected_Component | N_Slice =>
            Pre := Prefix (E);

            --  Fetch the original node when the prefix comes from the result
            --  of expanding a function call since we want to find the level
            --  of the original source call.

            if not Comes_From_Source (Pre)
              and then Nkind (Original_Node (Pre)) = N_Function_Call
            then
               Pre := Original_Node (Pre);
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
               return Make_Level_Literal
                        (Typ_Access_Level (Etype (Pre)));

            --  The current expression is a named access type, so there is no
            --  reason to look at the prefix. Instead obtain the level of E's
            --  named access type.

            elsif Is_Named_Access_Type (Etype (E)) then
               return Make_Level_Literal
                        (Typ_Access_Level (Etype (E)));

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
                  return Make_Level_Literal
                           (Typ_Access_Level (Etype (E)));
               end if;

               --  Otherwise proceed normally

               return Make_Level_Literal
                        (Typ_Access_Level (Etype (Prefix (E))));

            --  The accessibility calculation routine that handles function
            --  calls (Function_Call_Level) assumes, in the case the
            --  result is of an anonymous access type, that the result will be
            --  used "in its entirety" when the call is present within an
            --  assignment or object declaration.

            --  To properly handle cases where the result is not used in its
            --  entirety, we test if the prefix of the component in question is
            --  a function call, which tells us that one of its components has
            --  been identified and is being accessed. Therefore we can
            --  conclude that the result is not used "in its entirety"
            --  according to RM 3.10.2 (10.2/3).

            elsif Nkind (Pre) = N_Function_Call
              and then not Is_Named_Access_Type (Etype (Pre))
            then
               --  Dynamic checks are generated when we are within a return
               --  value or we are in a function call within an anonymous
               --  access discriminant constraint of a return object (signified
               --  by In_Return_Context) on the side of the callee.

               --  So, in this case, return a library accessibility level to
               --  null out the check on the side of the caller.

               if (In_Return_Value (E)
                    or else In_Return_Context)
                 and then Level /= Dynamic_Level
               then
                  return Make_Level_Literal
                           (Scope_Depth (Standard_Standard));
               end if;

               return Make_Level_Literal
                        (Innermost_Master_Scope_Depth (Expr));

            --  Otherwise, continue recursing over the expression prefixes

            else
               return Accessibility_Level (Prefix (E));
            end if;

         --  Qualified expressions

         when N_Qualified_Expression =>
            if Is_Named_Access_Type (Etype (E)) then
               return Make_Level_Literal
                        (Typ_Access_Level (Etype (E)));
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
               return Make_Level_Literal (Typ_Access_Level (Etype (Pre)));

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
               return Make_Level_Literal
                        (Typ_Access_Level (Etype (E)));

            --  In section RM 3.10.2 (10/4) the accessibility rules for
            --  aggregates and value conversions are outlined. Are these
            --  followed in the case of initialization of an object ???

            --  Should use Innermost_Master_Scope_Depth ???

            else
               return Accessibility_Level (Current_Scope);
            end if;

         --  Default to the type accessibility level for the type of the
         --  expression's entity.

         when others =>
            return Make_Level_Literal (Typ_Access_Level (Etype (E)));
      end case;
   end Accessibility_Level;

   -------------------------------
   -- Apply_Accessibility_Check --
   -------------------------------

   procedure Apply_Accessibility_Check
     (N           : Node_Id;
      Typ         : Entity_Id;
      Insert_Node : Node_Id)
   is
      Loc : constant Source_Ptr := Sloc (N);

      Check_Cond  : Node_Id;
      Param_Ent   : Entity_Id := Param_Entity (N);
      Param_Level : Node_Id;
      Type_Level  : Node_Id;

   begin
      --  Verify we haven't tried to add a dynamic accessibility check when we
      --  shouldn't.

      pragma Assert (not No_Dynamic_Accessibility_Checks_Enabled (N));

      if Ada_Version >= Ada_2012
         and then No (Param_Ent)
         and then Is_Entity_Name (N)
         and then Ekind (Entity (N)) in E_Constant | E_Variable
         and then Present (Effective_Extra_Accessibility (Entity (N)))
      then
         Param_Ent := Entity (N);
         while Present (Renamed_Object (Param_Ent)) loop
            --  Renamed_Object must return an Entity_Name here
            --  because of preceding "Present (E_E_A (...))" test.

            Param_Ent := Entity (Renamed_Object (Param_Ent));
         end loop;
      end if;

      if Inside_A_Generic then
         return;

      --  Only apply the run-time check if the access parameter has an
      --  associated extra access level parameter and when accessibility checks
      --  are enabled.

      elsif Present (Param_Ent)
         and then Present (Get_Dynamic_Accessibility (Param_Ent))
         and then not Accessibility_Checks_Suppressed (Param_Ent)
         and then not Accessibility_Checks_Suppressed (Typ)
      then
         --  Obtain the parameter's accessibility level

         Param_Level :=
           New_Occurrence_Of (Get_Dynamic_Accessibility (Param_Ent), Loc);

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
            Type_Level :=
              Make_Integer_Literal (Loc, Deepest_Type_Access_Level (Typ));
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
   end Apply_Accessibility_Check;

   ---------------------------------------------
   -- Apply_Accessibility_Check_For_Allocator --
   ---------------------------------------------

   procedure Apply_Accessibility_Check_For_Allocator
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
          (Type_Access_Level (Etype (Exp)) > Type_Access_Level (PtrT)
            or else
              (Is_Class_Wide_Type (Etype (Exp))
                and then Scope (PtrT) /= Current_Scope))
      then
         --  If the allocator was built in place, Ref is already a reference
         --  to the access object initialized to the result of the allocator
         --  (see Exp_Ch6.Make_Build_In_Place_Call_In_Allocator). We call
         --  Remove_Side_Effects for cases where the build-in-place call may
         --  still be the prefix of the reference (to avoid generating
         --  duplicate calls). Otherwise, it is the entity associated with
         --  the object containing the address of the allocated object.

         if Built_In_Place then
            Remove_Side_Effects (Ref);
            Obj_Ref := New_Copy_Tree (Ref);
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
             Right_Opnd => Accessibility_Level (N, Dynamic_Level));

         --  Due to the complexity and side effects of the check, utilize an if
         --  statement instead of the regular Program_Error circuitry.

         Insert_Action (N,
           Make_Implicit_If_Statement (N,
             Condition       => Cond,
             Then_Statements => Stmts));
      end if;
   end Apply_Accessibility_Check_For_Allocator;

   ------------------------------------------
   -- Check_Return_Construct_Accessibility --
   ------------------------------------------

   procedure Check_Return_Construct_Accessibility
     (Return_Stmt : Node_Id;
      Stm_Entity  : Entity_Id)
   is
      Loc      : constant Source_Ptr := Sloc (Return_Stmt);
      Scope_Id : constant Entity_Id  := Return_Applies_To (Stm_Entity);

      R_Type : constant Entity_Id := Etype (Scope_Id);
      --  Function result subtype

      function First_Selector (Assoc : Node_Id) return Node_Id;
      --  Obtain the first selector or choice from a given association

      function Is_Formal_Of_Current_Function
        (Assoc_Expr : Node_Id) return Boolean;
      --  Predicate to test if a given expression associated with a
      --  discriminant is a formal parameter to the function in which the
      --  return construct we checking applies to.

      --------------------
      -- First_Selector --
      --------------------

      function First_Selector (Assoc : Node_Id) return Node_Id is
      begin
         if Nkind (Assoc) = N_Component_Association then
            return First (Choices (Assoc));

         elsif Nkind (Assoc) = N_Discriminant_Association then
            return (First (Selector_Names (Assoc)));

         else
            raise Program_Error;
         end if;
      end First_Selector;

      -----------------------------------
      -- Is_Formal_Of_Current_Function --
      -----------------------------------

      function Is_Formal_Of_Current_Function
        (Assoc_Expr : Node_Id) return Boolean is
      begin
         return Is_Entity_Name (Assoc_Expr)
                  and then Enclosing_Subprogram
                             (Entity (Assoc_Expr)) = Scope_Id
                  and then Is_Formal (Entity (Assoc_Expr));
      end Is_Formal_Of_Current_Function;

      --  Local declarations

      Assoc : Node_Id := Empty;
      --  Assoc should perhaps be renamed and declared as a
      --  Node_Or_Entity_Id since it encompasses not only component and
      --  discriminant associations, but also discriminant components within
      --  a type declaration or subtype indication ???

      Assoc_Expr    : Node_Id;
      Assoc_Present : Boolean := False;

      Check_Cond        : Node_Id;
      Unseen_Disc_Count : Nat := 0;
      Seen_Discs        : Elist_Id;
      Disc              : Entity_Id;
      First_Disc        : Entity_Id;

      Obj_Decl   : Node_Id;
      Return_Con : Node_Id;
      Unqual     : Node_Id;

   --  Start of processing for Check_Return_Construct_Accessibility

   begin
      --  Only perform checks on record types with access discriminants and
      --  non-internally generated functions.

      if not Is_Record_Type (R_Type)
        or else not Has_Anonymous_Access_Discriminant (R_Type)
        or else not Comes_From_Source (Return_Stmt)
      then
         return;
      end if;

      --  We are only interested in return statements

      if Nkind (Return_Stmt) not in
           N_Extended_Return_Statement | N_Simple_Return_Statement
      then
         return;
      end if;

      --  Fetch the object from the return statement, in the case of a
      --  simple return statement the expression is part of the node.

      if Nkind (Return_Stmt) = N_Extended_Return_Statement then
         --  Obtain the object definition from the expanded extended return

         Return_Con := First (Return_Object_Declarations (Return_Stmt));
         while Present (Return_Con) loop
            --  Inspect the original node to avoid object declarations
            --  expanded into renamings.

            if Nkind (Original_Node (Return_Con)) = N_Object_Declaration
              and then Comes_From_Source (Original_Node (Return_Con))
            then
               exit;
            end if;

            Nlists.Next (Return_Con);
         end loop;

         pragma Assert (Present (Return_Con));

         --  Could be dealing with a renaming

         Return_Con := Original_Node (Return_Con);
      else
         Return_Con := Expression (Return_Stmt);
      end if;

      --  Obtain the accessibility levels of the expressions associated
      --  with all anonymous access discriminants, then generate a
      --  dynamic check or static error when relevant.

      --  Note the repeated use of Original_Node to avoid checking
      --  expanded code.

      Unqual := Original_Node (Unqualify (Original_Node (Return_Con)));

      --  Get the corresponding declaration based on the return object's
      --  identifier.

      if Nkind (Unqual) = N_Identifier
        and then Nkind (Parent (Entity (Unqual)))
                   in N_Object_Declaration
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

      if Present (Obj_Decl)
        and then Nkind (Obj_Decl) = N_Object_Renaming_Declaration
      then
         while Nkind (Obj_Decl) = N_Object_Renaming_Declaration loop

            if Nkind (Name (Obj_Decl)) not in N_Entity then
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
                             (Declaration_Node (Name (Obj_Decl)));
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

         --  Check the accessibility level of the expression when the
         --  discriminant is of an anonymous access type.

         if Present (Assoc_Expr)
           and then Present (Disc)
           and then Ekind (Etype (Disc)) = E_Anonymous_Access_Type

           --  We disable the check when we have a tagged return type and
           --  the associated expression for the discriminant is a formal
           --  parameter since the check would require us to compare the
           --  accessibility level of Assoc_Expr to the level of the
           --  Extra_Accessibility_Of_Result of the function - which is
           --  currently disabled for functions with tagged return types.
           --  This may change in the future ???

           --  See Needs_Result_Accessibility_Level for details.

           and then not
             (No (Extra_Accessibility_Of_Result (Scope_Id))
               and then Is_Formal_Of_Current_Function (Assoc_Expr)
               and then Is_Tagged_Type (Etype (Scope_Id)))
         then
            --  Generate a dynamic check based on the extra accessibility of
            --  the result or the scope of the current function.

            Check_Cond :=
              Make_Op_Gt (Loc,
                Left_Opnd  => Accessibility_Level
                                (Expr              => Assoc_Expr,
                                 Level             => Dynamic_Level,
                                 In_Return_Context => True),
                Right_Opnd =>
                  (if Present (Extra_Accessibility_Of_Result (Scope_Id))

                     --  When Assoc_Expr is a formal we have to look at the
                     --  extra accessibility-level formal associated with
                     --  the result.

                     and then Is_Formal_Of_Current_Function (Assoc_Expr)
                   then
                      New_Occurrence_Of
                        (Extra_Accessibility_Of_Result (Scope_Id), Loc)

                   --  Otherwise, we compare the level of Assoc_Expr to the
                   --  scope of the current function.

                   else
                      Make_Integer_Literal
                        (Loc, Scope_Depth (Scope (Scope_Id)))));

            Insert_Before_And_Analyze (Return_Stmt,
              Make_Raise_Program_Error (Loc,
                Condition => Check_Cond,
                Reason    => PE_Accessibility_Check_Failed));

            --  If constant folding has happened on the condition for the
            --  generated error, then warn about it being unconditional when
            --  we know an error will be raised.

            if Nkind (Check_Cond) = N_Identifier
              and then Entity (Check_Cond) = Standard_True
            then
               Error_Msg_N
                 ("access discriminant in return object would be a dangling"
                  & " reference", Return_Stmt);
            end if;
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
            Nlists.Next (Assoc);
         end if;
      end loop;
   end Check_Return_Construct_Accessibility;

   -------------------------------
   -- Deepest_Type_Access_Level --
   -------------------------------

   function Deepest_Type_Access_Level
     (Typ             : Entity_Id;
      Allow_Alt_Model : Boolean := True) return Uint
   is
   begin
      if Ekind (Typ) = E_Anonymous_Access_Type
        and then not Is_Local_Anonymous_Access (Typ)
        and then Nkind (Associated_Node_For_Itype (Typ)) = N_Object_Declaration
      then
         --  No_Dynamic_Accessibility_Checks override for alternative
         --  accessibility model.

         if Allow_Alt_Model
           and then No_Dynamic_Accessibility_Checks_Enabled (Typ)
         then
            return Type_Access_Level (Typ, Allow_Alt_Model);
         end if;

         --  Typ is the type of an Ada 2012 stand-alone object of an anonymous
         --  access type.

         return
           Scope_Depth (Enclosing_Dynamic_Scope
                         (Defining_Identifier
                           (Associated_Node_For_Itype (Typ))));

      --  For generic formal type, return Int'Last (infinite).
      --  See comment preceding Is_Generic_Type call in Type_Access_Level.

      elsif Is_Generic_Type (Root_Type (Typ)) then
         return UI_From_Int (Int'Last);

      else
         return Type_Access_Level (Typ, Allow_Alt_Model);
      end if;
   end Deepest_Type_Access_Level;

   -----------------------------------
   -- Effective_Extra_Accessibility --
   -----------------------------------

   function Effective_Extra_Accessibility (Id : Entity_Id) return Entity_Id is
   begin
      if Present (Renamed_Object (Id))
        and then Is_Entity_Name (Renamed_Object (Id))
      then
         return Effective_Extra_Accessibility (Entity (Renamed_Object (Id)));
      else
         return Extra_Accessibility (Id);
      end if;
   end Effective_Extra_Accessibility;

   -------------------------------
   -- Get_Dynamic_Accessibility --
   -------------------------------

   function Get_Dynamic_Accessibility (E : Entity_Id) return Entity_Id is
   begin
      --  When minimum accessibility is set for E then we utilize it - except
      --  in a few edge cases like the expansion of select statements where
      --  generated subprogram may attempt to unnecessarily use a minimum
      --  accessibility object declared outside of scope.

      --  To avoid these situations where expansion may get complex we verify
      --  that the minimum accessibility object is within scope.

      if Is_Formal (E)
        and then Present (Minimum_Accessibility (E))
        and then In_Open_Scopes (Scope (Minimum_Accessibility (E)))
      then
         return Minimum_Accessibility (E);
      end if;

      return Extra_Accessibility (E);
   end Get_Dynamic_Accessibility;

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

   --------------------------------
   -- Is_Anonymous_Access_Actual --
   --------------------------------

   function Is_Anonymous_Access_Actual (N : Node_Id) return Boolean is
      Par : Node_Id;
   begin
      if Ekind (Etype (N)) /= E_Anonymous_Access_Type then
         return False;
      end if;

      Par := Parent (N);
      while Present (Par)
        and then Nkind (Par) in N_Case_Expression
                              | N_If_Expression
                              | N_Parameter_Association
      loop
         Par := Parent (Par);
      end loop;
      return Nkind (Par) in N_Subprogram_Call;
   end Is_Anonymous_Access_Actual;

   --------------------------------------
   -- Is_Special_Aliased_Formal_Access --
   --------------------------------------

   function Is_Special_Aliased_Formal_Access
     (Exp               : Node_Id;
      In_Return_Context : Boolean := False) return Boolean
   is
      Scop : constant Entity_Id := Current_Subprogram;
   begin
      --  Verify the expression is an access reference to 'Access within a
      --  return statement as this is the only time an explicitly aliased
      --  formal has different semantics.

      if Nkind (Exp) /= N_Attribute_Reference
        or else Get_Attribute_Id (Attribute_Name (Exp)) /= Attribute_Access
        or else not (In_Return_Value (Exp)
                      or else In_Return_Context)
        or else not Needs_Result_Accessibility_Level (Scop)
      then
         return False;
      end if;

      --  Check if the prefix of the reference is indeed an explicitly aliased
      --  formal parameter for the function Scop. Additionally, we must check
      --  that Scop returns an anonymous access type, otherwise the special
      --  rules dictating a need for a dynamic check are not in effect.

      return Is_Entity_Name (Prefix (Exp))
               and then Is_Explicitly_Aliased (Entity (Prefix (Exp)));
   end Is_Special_Aliased_Formal_Access;

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

   -----------------------------
   -- Subprogram_Access_Level --
   -----------------------------

   function Subprogram_Access_Level (Subp : Entity_Id) return Uint is
   begin
      if Present (Alias (Subp)) then
         return Subprogram_Access_Level (Alias (Subp));
      else
         return Scope_Depth (Enclosing_Dynamic_Scope (Subp));
      end if;
   end Subprogram_Access_Level;

   --------------------------------
   -- Static_Accessibility_Level --
   --------------------------------

   function Static_Accessibility_Level
     (Expr              : Node_Id;
      Level             : Static_Accessibility_Level_Kind;
      In_Return_Context : Boolean := False) return Uint
   is
   begin
      return Intval
               (Accessibility_Level (Expr, Level, In_Return_Context));
   end Static_Accessibility_Level;

   -----------------------
   -- Type_Access_Level --
   -----------------------

   function Type_Access_Level
     (Typ             : Entity_Id;
      Allow_Alt_Model : Boolean   := True;
      Assoc_Ent       : Entity_Id := Empty) return Uint
   is
      Btyp    : Entity_Id := Base_Type (Typ);
      Def_Ent : Entity_Id;

   begin
      --  Ada 2005 (AI-230): For most cases of anonymous access types, we
      --  simply use the level where the type is declared. This is true for
      --  stand-alone object declarations, and for anonymous access types
      --  associated with components the level is the same as that of the
      --  enclosing composite type. However, special treatment is needed for
      --  the cases of access parameters, return objects of an anonymous access
      --  type, and, in Ada 95, access discriminants of limited types.

      if Is_Access_Type (Btyp) then
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

               --  When an anonymous access type's Assoc_Ent is specified,
               --  calculate the result based on the general accessibility
               --  level routine.

               --  We would like to use Associated_Node_For_Itype here instead,
               --  but in some cases it is not fine grained enough ???

               if Present (Assoc_Ent) then
                  return Static_Accessibility_Level
                           (Assoc_Ent, Object_Decl_Level);
               end if;

               --  Otherwise take the context of the anonymous access type into
               --  account.

               --  Obtain the defining entity for the internally generated
               --  anonymous access type.

               Def_Ent := Defining_Entity_Or_Empty
                            (Associated_Node_For_Itype (Typ));

               if Present (Def_Ent) then
                  --  When the defining entity is a subprogram then we know the
                  --  anonymous access type Typ has been generated to either
                  --  describe an anonymous access type formal or an anonymous
                  --  access result type.

                  --  Since we are only interested in the formal case, avoid
                  --  the anonymous access result type.

                  if Is_Subprogram (Def_Ent)
                    and then not (Ekind (Def_Ent) = E_Function
                                   and then Etype (Def_Ent) = Typ)
                  then
                     --  When the type comes from an anonymous access
                     --  parameter, the level is that of the subprogram
                     --  declaration.

                     return Scope_Depth (Def_Ent);

                  --  When the type is an access discriminant, the level is
                  --  that of the type.

                  elsif Ekind (Def_Ent) = E_Discriminant then
                     return Scope_Depth (Scope (Def_Ent));
                  end if;
               end if;

            --  If the type is a nonlocal anonymous access type (such as for
            --  an access parameter) we treat it as being declared at the
            --  library level to ensure that names such as X.all'access don't
            --  fail static accessibility checks.

            elsif not Is_Local_Anonymous_Access (Typ) then
               return Scope_Depth (Standard_Standard);

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
                  and then Is_Return_Object
                             (Defining_Identifier
                                (Associated_Node_For_Itype (Btyp))))
            then
               declare
                  Scop : Entity_Id;

               begin
                  Scop := Scope (Scope (Btyp));
                  while Present (Scop) loop
                     exit when Ekind (Scop) = E_Function;
                     Scop := Scope (Scop);
                  end loop;

                  --  Treat the return object's type as having the level of the
                  --  function's result subtype (as per RM05-6.5(5.3/2)).

                  return Type_Access_Level (Etype (Scop), Allow_Alt_Model);
               end;
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
            return Scope_Depth (Enclosing_Dynamic_Scope (Btyp)) + 1;
         end if;
      end if;

      --  Return library level for a generic formal type. This is done because
      --  RM(10.3.2) says that "The statically deeper relationship does not
      --  apply to ... a descendant of a generic formal type". Rather than
      --  checking at each point where a static accessibility check is
      --  performed to see if we are dealing with a formal type, this rule is
      --  implemented by having Type_Access_Level and Deepest_Type_Access_Level
      --  return extreme values for a formal type; Deepest_Type_Access_Level
      --  returns Int'Last. By calling the appropriate function from among the
      --  two, we ensure that the static accessibility check will pass if we
      --  happen to run into a formal type. More specifically, we should call
      --  Deepest_Type_Access_Level instead of Type_Access_Level whenever the
      --  call occurs as part of a static accessibility check and the error
      --  case is the case where the type's level is too shallow (as opposed
      --  to too deep).

      if Is_Generic_Type (Root_Type (Btyp)) then
         return Scope_Depth (Standard_Standard);
      end if;

      return Scope_Depth (Enclosing_Dynamic_Scope (Btyp));
   end Type_Access_Level;

end Accessibility;
