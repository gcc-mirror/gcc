------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ W A R N                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2003 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Alloc;
with Atree;    use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Fname;    use Fname;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Table;

package body Sem_Warn is

   --  The following table collects Id's of entities that are potentially
   --  unreferenced. See Check_Unset_Reference for further details.

   package Unreferenced_Entities is new Table.Table (
     Table_Component_Type => Entity_Id,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.Unreferenced_Entities_Initial,
     Table_Increment      => Alloc.Unreferenced_Entities_Increment,
     Table_Name           => "Unreferenced_Entities");

   ------------------------------
   -- Handling of Conditionals --
   ------------------------------

   --  Note: this is work in progress, the data structures and general
   --  approach are defined, but are not in use yet. ???

   --  One entry is made in the following table for each branch of
   --  a conditional, e.g. an if-then-elsif-else-endif structure
   --  creates three entries in this table.

   type Branch_Entry is record
      Sloc : Source_Ptr;
      --  Location for warnings associated with this branch

      Defs : Elist_Id;
      --  List of entities defined for the first time in this branch. On
      --  exit from a conditional structure, any entity that is in the
      --  list of all branches is removed (and the entity flagged as
      --  defined by the conditional as a whole). Thus after processing
      --  a conditional, Defs contains a list of entities defined in this
      --  branch for the first time, but not defined at all in some other
      --  branch of the same conditional. A value of No_Elist is used to
      --  represent the initial empty list.

      Next : Nat;
      --  Index of next branch for this conditional, zero = last branch
   end record;

   package Branch_Table is new Table.Table (
     Table_Component_Type => Branch_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.Branches_Initial,
     Table_Increment      => Alloc.Branches_Increment,
     Table_Name           => "Branches");

   --  The following table is used to represent conditionals, there is
   --  one entry in this table for each conditional structure.

   type Conditional_Entry is record
      If_Stmt : Boolean;
      --  True for IF statement, False for CASE statement

      First_Branch : Nat;
      --  Index in Branch table of first branch, zero = none yet

      Current_Branch : Nat;
      --  Index in Branch table of current branch, zero = none yet
   end record;

   package Conditional_Table is new Table.Table (
     Table_Component_Type => Conditional_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.Conditionals_Initial,
     Table_Increment      => Alloc.Conditionals_Increment,
     Table_Name           => "Conditionals");

   --  The following table is a stack that keeps track of the current
   --  conditional. The Last entry is the top of the stack. An Empty
   --  entry represents the start of a compilation unit. Non-zero
   --  entries in the stack are indexes into the conditional table.

   package Conditional_Stack is new Table.Table (
     Table_Component_Type => Nat,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.Conditional_Stack_Initial,
     Table_Increment      => Alloc.Conditional_Stack_Increment,
     Table_Name           => "Conditional_Stack");

   pragma Warnings (Off, Branch_Table);
   pragma Warnings (Off, Conditional_Table);
   pragma Warnings (Off, Conditional_Stack);
   --  Not yet referenced, see note above ???

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Generic_Package_Spec_Entity (E : Entity_Id) return Boolean;
   --  This returns true if the entity E is declared within a generic package.
   --  The point of this is to detect variables which are not assigned within
   --  the generic, but might be assigned outside the package for any given
   --  instance. These are cases where we leave the warnings to be posted
   --  for the instance, when we will know more.

   function Operand_Has_Warnings_Suppressed (N : Node_Id) return Boolean;
   --  This function traverses the expression tree represented by the node
   --  N and determines if any sub-operand is a reference to an entity for
   --  which the Warnings_Off flag is set. True is returned if such an
   --  entity is encountered, and False otherwise.

   ----------------------
   -- Check_References --
   ----------------------

   procedure Check_References (E : Entity_Id; Anod : Node_Id := Empty) is
      E1 : Entity_Id;
      UR : Node_Id;

      function Missing_Subunits return Boolean;
      --  We suppress warnings when there are missing subunits, because this
      --  may generate too many false positives: entities in a parent may
      --  only be referenced in one of the subunits. We make an exception
      --  for subunits that contain no other stubs.

      procedure Output_Reference_Error (M : String);
      --  Used to output an error message. Deals with posting the error on
      --  the body formal in the accept case.

      function Publicly_Referenceable (Ent : Entity_Id) return Boolean;
      --  This is true if the entity in question is potentially referenceable
      --  from another unit. This is true for entities in packages that are
      --  at the library level.

      -----------------------
      --  Missing_Subunits --
      -----------------------

      function Missing_Subunits return Boolean is
         D : Node_Id;

      begin
         if not Unloaded_Subunits then

            --  Normal compilation, all subunits are present

            return False;

         elsif E /= Main_Unit_Entity then

            --  No warnings on a stub that is not the main unit

            return True;

         elsif Nkind (Unit_Declaration_Node (E)) in N_Proper_Body then
            D := First (Declarations (Unit_Declaration_Node (E)));

            while Present (D) loop

               --  No warnings if the proper body contains nested stubs

               if Nkind (D) in N_Body_Stub then
                  return True;
               end if;

               Next (D);
            end loop;

            return False;

         else
            --   Missing stubs elsewhere

            return True;
         end if;
      end Missing_Subunits;

      ----------------------------
      -- Output_Reference_Error --
      ----------------------------

      procedure Output_Reference_Error (M : String) is
      begin
         --  Other than accept case, post error on defining identifier

         if No (Anod) then
            Error_Msg_N (M, E1);

         --  Accept case, find body formal to post the message

         else
            declare
               Parm  : Node_Id;
               Enod  : Node_Id;
               Defid : Entity_Id;

            begin
               Enod := Anod;

               if Present (Parameter_Specifications (Anod)) then
                  Parm := First (Parameter_Specifications (Anod));

                  while Present (Parm) loop
                     Defid := Defining_Identifier (Parm);

                     if Chars (E1) = Chars (Defid) then
                        Enod := Defid;
                        exit;
                     end if;

                     Next (Parm);
                  end loop;
               end if;

               Error_Msg_NE (M, Enod, E1);
            end;
         end if;
      end Output_Reference_Error;

      ----------------------------
      -- Publicly_Referenceable --
      ----------------------------

      function Publicly_Referenceable (Ent : Entity_Id) return Boolean is
         P    : Node_Id;
         Prev : Node_Id;

      begin
         --  Examine parents to look for a library level package spec
         --  But if we find a body or block or other similar construct
         --  along the way, we cannot be referenced.

         Prev := Ent;
         P    := Parent (Ent);
         loop
            case Nkind (P) is

               --  If we get to top of tree, then publicly referenceable

               when N_Empty =>
                  return True;

               --  If we reach a generic package declaration, then always
               --  consider this referenceable, since any instantiation will
               --  have access to the entities in the generic package. Note
               --  that the package itself may not be instantiated, but then
               --  we will get a warning for the package entity
               --  Note that generic formal parameters are themselves not
               --  publicly referenceable in an instance, and warnings on
               --  them are useful.

               when N_Generic_Package_Declaration =>
                  return
                    not Is_List_Member (Prev)
                      or else List_Containing (Prev)
                        /= Generic_Formal_Declarations (P);

               --  if we reach a subprogram body, entity is not referenceable
               --  unless it is the defining entity of the body. This will
               --  happen, e.g. when a function is an attribute renaming that
               --  is rewritten as a body.

               when N_Subprogram_Body  =>
                  if Ent /= Defining_Entity (P) then
                     return False;
                  else
                     P := Parent (P);
                  end if;

               --  If we reach any other body, definitely not referenceable

               when N_Package_Body    |
                    N_Task_Body       |
                    N_Entry_Body      |
                    N_Protected_Body  |
                    N_Block_Statement |
                    N_Subunit         =>
                  return False;

               --  For all other cases, keep looking up tree

               when others =>
                  Prev := P;
                  P    := Parent (P);
            end case;
         end loop;
      end Publicly_Referenceable;

   --  Start of processing for Check_References

   begin
      --  No messages if warnings are suppressed, or if we have detected
      --  any real errors so far (this last check avoids junk messages
      --  resulting from errors, e.g. a subunit that is not loaded).

      if Warning_Mode = Suppress
        or else Serious_Errors_Detected /= 0
      then
         return;
      end if;

      --  We also skip the messages if any subunits were not loaded (see
      --  comment in Sem_Ch10 to understand how this is set, and why it is
      --  necessary to suppress the warnings in this case).

      if Missing_Subunits then
         return;
      end if;

      --  Otherwise loop through entities, looking for suspicious stuff

      E1 := First_Entity (E);
      while Present (E1) loop

         --  We only look at source entities with warning flag off

         if Comes_From_Source (E1) and then not Warnings_Off (E1) then

            --  We are interested in variables and out parameters, but we
            --  exclude protected types, too complicated to worry about.

            if Ekind (E1) = E_Variable
                 or else
               (Ekind (E1) = E_Out_Parameter
                  and then not Is_Protected_Type (Current_Scope))
            then
               --  Post warning if this object not assigned. Note that we
               --  do not consider the implicit initialization of an access
               --  type to be the assignment of a value for this purpose.

               --  If the entity is an out parameter of the current subprogram
               --  body, check the warning status of the parameter in the spec.

               if Ekind (E1) = E_Out_Parameter
                 and then Present (Spec_Entity (E1))
                 and then Warnings_Off (Spec_Entity (E1))
               then
                  null;

               elsif Never_Set_In_Source (E1)
                 and then not Generic_Package_Spec_Entity (E1)
               then
                  if Warn_On_No_Value_Assigned then

                     --  Do not output complaint about never being assigned a
                     --  value if a pragma Unreferenced applies to the variable
                     --  or if it is a parameter, to the corresponding spec.

                     if Has_Pragma_Unreferenced (E1)
                       or else (Is_Formal (E1)
                                  and then Present (Spec_Entity (E1))
                                  and then
                                    Has_Pragma_Unreferenced (Spec_Entity (E1)))
                     then
                        null;

                     --  Pragma Unreferenced not set, so output message

                     else
                        Output_Reference_Error
                          ("& is never assigned a value?");

                        --  Deal with special case where this variable is
                        --  hidden by a loop variable

                        if Ekind (E1) = E_Variable
                          and then Present (Hiding_Loop_Variable (E1))
                        then
                           Error_Msg_Sloc := Sloc (E1);
                           Error_Msg_N
                             ("declaration hides &#?",
                              Hiding_Loop_Variable (E1));
                           Error_Msg_N
                             ("for loop implicitly declares loop variable?",
                              Hiding_Loop_Variable (E1));
                        end if;
                     end if;
                  end if;
                  goto Continue;

               --  Case of variable that could be a constant. Note that we
               --  never signal such messages for generic package entities,
               --  since a given instance could have modifications outside
               --  the package.

               elsif Warn_On_Constant
                 and then Ekind (E1) = E_Variable
                 and then Is_True_Constant (E1)
                 and then not Generic_Package_Spec_Entity (E1)
               then
                  Error_Msg_N
                    ("& is not modified, could be declared constant?", E1);
               end if;

               --  Check for unset reference, note that we exclude access
               --  types from this check, since access types do always have
               --  a null value, and that seems legitimate in this case.

               if Ekind (E1) = E_Out_Parameter
                 and then Present (Spec_Entity (E1))
               then
                  UR := Unset_Reference (Spec_Entity (E1));
               else
                  UR := Unset_Reference (E1);
               end if;

               if Warn_On_No_Value_Assigned and then Present (UR) then

                  --  For access types, the only time we made a UR entry
                  --  was for a dereference, and so we post the appropriate
                  --  warning here. The issue is not that the value is not
                  --  initialized here, but that it is null.

                  if Is_Access_Type (Etype (E1)) then
                     Error_Msg_NE ("& may be null?", UR, E1);
                     goto Continue;

                  --  For other than access type, go back to original node
                  --  to deal with case where original unset reference
                  --  has been rewritten during expansion.

                  else
                     UR := Original_Node (UR);

                     --  In some cases, the original node may be a type
                     --  conversion or qualification, and in this case
                     --  we want the object entity inside.

                     while Nkind (UR) = N_Type_Conversion
                       or else Nkind (UR) = N_Qualified_Expression
                     loop
                        UR := Expression (UR);
                     end loop;

                     --  Here we issue the warning, all checks completed

                     if Nkind (Parent (UR)) = N_Selected_Component then
                        Error_Msg_Node_2 := Selector_Name (Parent (UR));
                        Error_Msg_N
                          ("`&.&` may be referenced before it has a value?",
                           UR);
                     else
                        Error_Msg_N
                          ("& may be referenced before it has a value?",
                           UR);
                     end if;

                     goto Continue;
                  end if;
               end if;
            end if;

            --  Then check for unreferenced entities. Note that we are only
            --  interested in entities which do not have the Referenced flag
            --  set. The Referenced_As_LHS flag is interesting only if the
            --  Referenced flag is not set.

            if not Referenced (E1)

               --  Check that warnings on unreferenced entities are enabled

              and then ((Check_Unreferenced and then not Is_Formal (E1))
                           or else
                        (Check_Unreferenced_Formals and then Is_Formal (E1))
                           or else
                        (Warn_On_Modified_Unread
                          and then Referenced_As_LHS (E1)))

               --  Labels, and enumeration literals, and exceptions. The
               --  warnings are also placed on local packages that cannot
               --  be referenced from elsewhere, including those declared
               --  within a package body.

               and then (Is_Object (E1)
                           or else
                         Is_Type (E1)
                           or else
                         Ekind (E1) = E_Label
                           or else
                         Ekind (E1) = E_Exception
                           or else
                         Ekind (E1) = E_Named_Integer
                           or else
                         Ekind (E1) = E_Named_Real
                           or else
                         Is_Overloadable (E1)
                           or else
                             (Ekind (E1) = E_Package
                               and then
                                (Ekind (E) = E_Function
                                  or else Ekind (E) = E_Package_Body
                                  or else Ekind (E) = E_Procedure
                                  or else Ekind (E) = E_Block)))

               --  Exclude instantiations, since there is no reason why
               --  every entity in an instantiation should be referenced.

               and then Instantiation_Location (Sloc (E1)) = No_Location

               --  Exclude formal parameters from bodies if the corresponding
               --  spec entity has been referenced in the case where there is
               --  a separate spec.

               and then not (Is_Formal (E1)
                               and then
                             Ekind (Scope (E1)) = E_Subprogram_Body
                               and then
                             Present (Spec_Entity (E1))
                               and then
                             Referenced (Spec_Entity (E1)))

               --  Consider private type referenced if full view is referenced
               --  If there is not full view, this is a generic type on which
               --  warnings are also useful.

               and then
                 not (Is_Private_Type (E1)
                   and then
                     Present (Full_View (E1))
                       and then Referenced (Full_View (E1)))

               --  Don't worry about full view, only about private type

               and then not Has_Private_Declaration (E1)

               --  Eliminate dispatching operations from consideration, we
               --  cannot tell if these are referenced or not in any easy
               --  manner (note this also catches Adjust/Finalize/Initialize)

               and then not Is_Dispatching_Operation (E1)

               --  Check entity that can be publicly referenced (we do not
               --  give messages for such entities, since there could be
               --  other units, not involved in this compilation, that
               --  contain relevant references.

               and then not Publicly_Referenceable (E1)

               --  Class wide types are marked as source entities, but
               --  they are not really source entities, and are always
               --  created, so we do not care if they are not referenced.

               and then Ekind (E1) /= E_Class_Wide_Type

               --  Objects other than parameters of task types are allowed
               --  to be non-referenced, since they start up tasks!

               and then ((Ekind (E1) /= E_Variable
                             and then Ekind (E1) /= E_Constant
                             and then Ekind (E1) /= E_Component)
                           or else not Is_Task_Type (Etype (E1)))

               --  For subunits, only place warnings on the main unit
               --  itself, since parent units are not completely compiled

               and then (Nkind (Unit (Cunit (Main_Unit))) /= N_Subunit
                           or else
                         Get_Source_Unit (E1) = Main_Unit)
            then
               --  Suppress warnings in internal units if not in -gnatg
               --  mode (these would be junk warnings for an applications
               --  program, since they refer to problems in internal units)

               if GNAT_Mode
                 or else not
                   Is_Internal_File_Name
                     (Unit_File_Name (Get_Source_Unit (E1)))
               then
                  --  We do not immediately flag the error. This is because
                  --  we have not expanded generic bodies yet, and they may
                  --  have the missing reference. So instead we park the
                  --  entity on a list, for later processing. However, for
                  --  the accept case, post the error right here, since we
                  --  have the information now in this case.

                  if Present (Anod) then
                     Output_Reference_Error ("& is not referenced?");

                  else
                     Unreferenced_Entities.Increment_Last;
                     Unreferenced_Entities.Table
                       (Unreferenced_Entities.Last) := E1;
                  end if;
               end if;

            --  Generic units are referenced in the generic body,
            --  but if they are not public and never instantiated
            --  we want to force a warning on them. We treat them
            --  as redundant constructs to minimize noise.

            elsif Is_Generic_Subprogram (E1)
              and then not Is_Instantiated (E1)
              and then not Publicly_Referenceable (E1)
              and then Instantiation_Depth (Sloc (E1)) = 0
              and then Warn_On_Redundant_Constructs
            then
               Unreferenced_Entities.Increment_Last;
               Unreferenced_Entities.Table (Unreferenced_Entities.Last) := E1;

               --  Force warning on entity.

               Set_Referenced (E1, False);
            end if;
         end if;

         --  Recurse into nested package or block. Do not recurse into a
         --  formal package, because the correponding body is not analyzed.

         <<Continue>>
            if ((Ekind (E1) = E_Package or else Ekind (E1) = E_Generic_Package)
                  and then Nkind (Parent (E1)) = N_Package_Specification
                  and then
                    Nkind (Original_Node (Unit_Declaration_Node (E1)))
                      /= N_Formal_Package_Declaration)

              or else Ekind (E1) = E_Block
            then
               Check_References (E1);
            end if;

            Next_Entity (E1);
      end loop;
   end Check_References;

   ---------------------------
   -- Check_Unset_Reference --
   ---------------------------

   procedure Check_Unset_Reference (N : Node_Id) is
   begin
      --  Nothing to do if warnings suppressed

      if Warning_Mode = Suppress then
         return;
      end if;

      --  Ignore reference to non-scalar if not from source. Almost always
      --  such references are bogus (e.g. calls to init procs to set
      --  default discriminant values).

      if not Comes_From_Source (N)
        and then not Is_Scalar_Type (Etype (N))
      then
         return;
      end if;

      --  Otherwise see what kind of node we have. If the entity already
      --  has an unset reference, it is not necessarily the earliest in
      --  the text, because resolution of the prefix of selected components
      --  is completed before the resolution of the selected component itself.
      --  as a result, given  (R /= null and then R.X > 0), the occurrences
      --  of R are examined in right-to-left order. If there is already an
      --  unset reference, we check whether N is earlier before proceeding.

      case Nkind (N) is
         when N_Identifier | N_Expanded_Name =>
            declare
               E : constant Entity_Id := Entity (N);

            begin
               if (Ekind (E) = E_Variable
                    or else Ekind (E) = E_Out_Parameter)
                 and then Never_Set_In_Source (E)
                 and then (No (Unset_Reference (E))
                             or else Earlier_In_Extended_Unit
                               (Sloc (N),  Sloc (Unset_Reference (E))))
                 and then not Warnings_Off (E)
               then
                  --  We may have an unset reference. The first test is
                  --  whether we are accessing a discriminant of a record
                  --  or a component with default initialization. Both of
                  --  these cases can be ignored, since the actual object
                  --  that is referenced is definitely initialized. Note
                  --  that this covers the case of reading discriminants
                  --  of an out parameter, which is OK even in Ada 83.

                  --  Note that we are only interested in a direct reference
                  --  to a record component here. If the reference is via an
                  --  access type, then the access object is being referenced,
                  --  not the record, and still deserves an unset reference.

                  if Nkind (Parent (N)) = N_Selected_Component
                    and not Is_Access_Type (Etype (N))
                  then
                     declare
                        ES : constant Entity_Id :=
                               Entity (Selector_Name (Parent (N)));

                     begin
                        if Ekind (ES) = E_Discriminant
                          or else Present (Expression (Declaration_Node (ES)))
                        then
                           return;
                        end if;
                     end;
                  end if;

                  --  Here we have a potential unset reference. But before we
                  --  get worried about it, we have to make sure that the
                  --  entity declaration is in the same procedure as the
                  --  reference, since if they are in separate procedures,
                  --  then we have no idea about sequential execution.

                  --  The tests in the loop below catch all such cases, but
                  --  do allow the reference to appear in a loop, block, or
                  --  package spec that is nested within the declaring scope.
                  --  As always, it is possible to construct cases where the
                  --  warning is wrong, that is why it is a warning!

                  declare
                     SR : Entity_Id;
                     SE : constant Entity_Id := Scope (E);

                  begin
                     SR := Current_Scope;
                     while SR /= SE loop
                        if SR = Standard_Standard
                          or else Is_Subprogram (SR)
                          or else Is_Concurrent_Body (SR)
                          or else Is_Concurrent_Type (SR)
                        then
                           return;
                        end if;

                        SR := Scope (SR);
                     end loop;

                     --  Case of reference has an access type. This is a
                     --  special case since access types are always set to
                     --  null so cannot be truly uninitialized, but we still
                     --  want to warn about cases of obvious null dereference.

                     if Is_Access_Type (Etype (N)) then
                        declare
                           P : Node_Id;

                           function Process
                             (N    : Node_Id)
                              return Traverse_Result;
                           --  Process function for instantation of Traverse
                           --  below. Checks if N contains reference to E
                           --  other than a dereference.

                           function Ref_In (Nod : Node_Id) return Boolean;
                           --  Determines whether Nod contains a reference
                           --  to the entity E that is not a dereference.

                           function Process
                             (N    : Node_Id)
                              return Traverse_Result
                           is
                           begin
                              if Is_Entity_Name (N)
                                and then Entity (N) = E
                                and then not Is_Dereferenced (N)
                              then
                                 return Abandon;
                              else
                                 return OK;
                              end if;
                           end Process;

                           function Ref_In (Nod : Node_Id) return Boolean is
                              function Traverse is new Traverse_Func (Process);

                           begin
                              return Traverse (Nod) = Abandon;
                           end Ref_In;

                        begin
                           --  Don't bother if we are inside an instance,
                           --  since the compilation of the generic template
                           --  is where the warning should be issued.

                           if In_Instance then
                              return;
                           end if;

                           --  Don't bother if this is not the main unit.
                           --  If we try to give this warning for with'ed
                           --  units, we get some false positives, since
                           --  we do not record references in other units.

                           if not In_Extended_Main_Source_Unit (E)
                                or else
                              not In_Extended_Main_Source_Unit (N)
                           then
                              return;
                           end if;

                           --  We are only interested in deferences

                           if not Is_Dereferenced (N) then
                              return;
                           end if;

                           --  One more check, don't bother with references
                           --  that are inside conditional statements or while
                           --  loops if the condition references the entity in
                           --  question. This avoids most false positives.

                           P := Parent (N);
                           loop
                              P := Parent (P);
                              exit when No (P);

                              if (Nkind (P) = N_If_Statement
                                     or else
                                   Nkind (P) = N_Elsif_Part)
                                 and then Ref_In (Condition (P))
                              then
                                 return;

                              elsif Nkind (P) = N_Loop_Statement
                                and then Present (Iteration_Scheme (P))
                                and then
                                  Ref_In (Condition (Iteration_Scheme (P)))
                              then
                                 return;
                              end if;
                           end loop;
                        end;
                     end if;

                     --  Here we definitely have a case for giving a warning
                     --  for a reference to an unset value. But we don't give
                     --  the warning now. Instead we set the Unset_Reference
                     --  field of the identifier involved. The reason for this
                     --  is that if we find the variable is never ever assigned
                     --  a value then that warning is more important and there
                     --  is no point in giving the reference warning.

                     --  If this is an identifier, set the field directly

                     if Nkind (N) = N_Identifier then
                        Set_Unset_Reference (E, N);

                     --  Otherwise it is an expanded name, so set the field
                     --  of the actual identifier for the reference.

                     else
                        Set_Unset_Reference (E, Selector_Name (N));
                     end if;
                  end;
               end if;
            end;

         when N_Indexed_Component | N_Slice =>
            Check_Unset_Reference (Prefix (N));

         when N_Selected_Component =>

            if Present (Entity (Selector_Name (N)))
              and then Ekind (Entity (Selector_Name (N))) = E_Discriminant
            then
               --   A discriminant is always initialized

               null;

            else
               Check_Unset_Reference (Prefix (N));
            end if;

         when N_Type_Conversion | N_Qualified_Expression =>
            Check_Unset_Reference (Expression (N));

         when others =>
            null;

      end case;
   end Check_Unset_Reference;

   ------------------------
   -- Check_Unused_Withs --
   ------------------------

   procedure Check_Unused_Withs (Spec_Unit : Unit_Number_Type := No_Unit) is
      Cnode : Node_Id;
      Item  : Node_Id;
      Lunit : Node_Id;
      Ent   : Entity_Id;

      Munite : constant Entity_Id := Cunit_Entity (Main_Unit);
      --  This is needed for checking the special renaming case

      procedure Check_One_Unit (Unit : Unit_Number_Type);
      --  Subsidiary procedure, performs checks for specified unit

      --------------------
      -- Check_One_Unit --
      --------------------

      procedure Check_One_Unit (Unit : Unit_Number_Type) is
         Is_Visible_Renaming : Boolean := False;
         Pack                : Entity_Id;

         procedure Check_Inner_Package (Pack : Entity_Id);
         --  Pack is a package local to a unit in a with_clause. Both the
         --  unit and Pack are referenced. If none of the entities in Pack
         --  are referenced, then the only occurrence of Pack is in a use
         --  clause or a pragma, and a warning is worthwhile as well.

         function Check_System_Aux return Boolean;
         --  Before giving a warning on a with_clause for System, check
         --  whether a system extension is present.

         function Find_Package_Renaming
           (P : Entity_Id;
            L : Entity_Id) return Entity_Id;
         --  The only reference to a context unit may be in a renaming
         --  declaration. If this renaming declares a visible entity, do
         --  not warn that the context clause could be moved to the body,
         --  because the renaming may be intented to re-export the unit.

         -------------------------
         -- Check_Inner_Package --
         -------------------------

         procedure Check_Inner_Package (Pack : Entity_Id) is
            E  : Entity_Id;
            Un : constant Node_Id := Sinfo.Unit (Cnode);

            function Check_Use_Clause (N : Node_Id) return Traverse_Result;
            --  If N is a use_clause for Pack, emit warning.

            procedure Check_Use_Clauses is new
              Traverse_Proc (Check_Use_Clause);

            ----------------------
            -- Check_Use_Clause --
            ----------------------

            function Check_Use_Clause (N : Node_Id) return Traverse_Result is
               Nam  : Node_Id;

            begin
               if Nkind (N) = N_Use_Package_Clause then
                  Nam := First (Names (N));

                  while Present (Nam) loop
                     if Entity (Nam) = Pack then
                        Error_Msg_Qual_Level := 1;
                        Error_Msg_NE
                          ("no entities of package& are referenced?",
                             Nam, Pack);
                        Error_Msg_Qual_Level := 0;
                     end if;

                     Next (Nam);
                  end loop;
               end if;

               return OK;
            end Check_Use_Clause;

         --  Start of processing for Check_Inner_Package

         begin
            E := First_Entity (Pack);

            while Present (E) loop
               if Referenced (E) then
                  return;
               end if;

               Next_Entity (E);
            end loop;

            --  No entities of the package are referenced. Check whether
            --  the reference to the package itself is a use clause, and
            --  if so place a warning on it.

            Check_Use_Clauses (Un);
         end Check_Inner_Package;

         ----------------------
         -- Check_System_Aux --
         ----------------------

         function Check_System_Aux return Boolean is
            Ent : Entity_Id;

         begin
            if Chars (Lunit) = Name_System
               and then Scope (Lunit) = Standard_Standard
               and then Present_System_Aux
            then
               Ent := First_Entity (System_Aux_Id);

               while Present (Ent) loop
                  if Referenced (Ent) then
                     return True;
                  end if;

                  Next_Entity (Ent);
               end loop;
            end if;

            return False;
         end Check_System_Aux;

         ---------------------------
         -- Find_Package_Renaming --
         ---------------------------

         function Find_Package_Renaming
           (P : Entity_Id;
            L : Entity_Id) return Entity_Id
         is
            E1 : Entity_Id;
            R  : Entity_Id;

         begin
            Is_Visible_Renaming := False;
            E1 := First_Entity (P);

            while Present (E1) loop
               if Ekind (E1) = E_Package
                  and then Renamed_Object (E1) = L
               then
                  Is_Visible_Renaming := not Is_Hidden (E1);
                  return E1;

               elsif Ekind (E1) = E_Package
                 and then No (Renamed_Object (E1))
                 and then not Is_Generic_Instance (E1)
               then
                  R := Find_Package_Renaming (E1, L);

                  if Present (R) then
                     Is_Visible_Renaming := not Is_Hidden (R);
                     return R;
                  end if;
               end if;

               Next_Entity (E1);
            end loop;

            return Empty;
         end Find_Package_Renaming;

      --  Start of processing for Check_One_Unit

      begin
         Cnode := Cunit (Unit);

         --  Only do check in units that are part of the extended main
         --  unit. This is actually a necessary restriction, because in
         --  the case of subprogram acting as its own specification,
         --  there can be with's in subunits that we will not see.

         if not In_Extended_Main_Source_Unit (Cnode) then
            return;

         --  In configurable run time mode, we remove the bodies of
         --  non-inlined subprograms, which may lead to spurious warnings,
         --  which are clearly undesirable.

         elsif Configurable_Run_Time_Mode
           and then Is_Predefined_File_Name (Unit_File_Name (Unit))
         then
            return;
         end if;

         --  Loop through context items in this unit

         Item := First (Context_Items (Cnode));
         while Present (Item) loop
            if Nkind (Item) = N_With_Clause
               and then not Implicit_With (Item)
               and then In_Extended_Main_Source_Unit (Item)
            then
               Lunit := Entity (Name (Item));

               --  Check if this unit is referenced

               if not Referenced (Lunit) then

                  --  Suppress warnings in internal units if not in -gnatg
                  --  mode (these would be junk warnings for an applications
                  --  program, since they refer to problems in internal units)

                  if GNAT_Mode
                    or else not Is_Internal_File_Name (Unit_File_Name (Unit))
                  then
                     --  Here we definitely have a non-referenced unit. If
                     --  it is the special call for a spec unit, then just
                     --  set the flag to be read later.

                     if Unit = Spec_Unit then
                        Set_Unreferenced_In_Spec (Item);

                     --  Otherwise simple unreferenced message

                     else
                        Error_Msg_N
                          ("unit& is not referenced?", Name (Item));
                     end if;
                  end if;

               --  If main unit is a renaming of this unit, then we consider
               --  the with to be OK (obviously it is needed in this case!)

               elsif Present (Renamed_Entity (Munite))
                  and then Renamed_Entity (Munite) = Lunit
               then
                  null;

               --  If this unit is referenced, and it is a package, we
               --  do another test, to see if any of the entities in the
               --  package are referenced. If none of the entities are
               --  referenced, we still post a warning. This occurs if
               --  the only use of the package is in a use clause, or
               --  in a package renaming declaration.

               elsif Ekind (Lunit) = E_Package then

                  --  If Is_Instantiated is set, it means that the package
                  --  is implicitly instantiated (this is the case of a
                  --  parent instance or an actual for a generic package
                  --  formal), and this counts as a reference.

                  if Is_Instantiated (Lunit) then
                     null;

                  --  If no entities in package, and there is a pragma
                  --  Elaborate_Body present, then assume that this with
                  --  is done for purposes of this elaboration.

                  elsif No (First_Entity (Lunit))
                    and then Has_Pragma_Elaborate_Body (Lunit)
                  then
                     null;

                  --  Otherwise see if any entities have been referenced

                  else
                     Ent := First_Entity (Lunit);
                     loop
                        --  No more entities, and we did not find one
                        --  that was referenced. Means we have a definite
                        --  case of a with none of whose entities was
                        --  referenced.

                        if No (Ent) then

                           --  If in spec, just set the flag

                           if Unit = Spec_Unit then
                              Set_No_Entities_Ref_In_Spec (Item);

                           elsif Check_System_Aux then
                              null;

                           --  Else give the warning

                           else
                              Error_Msg_N
                                ("no entities of & are referenced?",
                                 Name (Item));

                              --  Look for renamings of this package, and
                              --  flag them as well. If the original package
                              --  has warnings off, we suppress the warning
                              --  on the renaming as well.

                              Pack := Find_Package_Renaming (Munite, Lunit);

                              if Present (Pack)
                                and then not Warnings_Off (Lunit)
                              then
                                 Error_Msg_NE
                                   ("no entities of & are referenced?",
                                     Unit_Declaration_Node (Pack),
                                       Pack);
                              end if;
                           end if;

                           exit;

                        --  Case of next entity is referenced

                        elsif Referenced (Ent)
                          or else Referenced_As_LHS (Ent)
                        then
                           --  This means that the with is indeed fine, in
                           --  that it is definitely needed somewhere, and
                           --  we can quite worrying about this one.

                           --  Except for one little detail, if either of
                           --  the flags was set during spec processing,
                           --  this is where we complain that the with
                           --  could be moved from the spec. If the spec
                           --  contains a visible renaming of the package,
                           --  inhibit warning to move with_clause to body.

                           if Ekind (Munite) = E_Package_Body then
                              Pack :=
                                Find_Package_Renaming
                                  (Spec_Entity (Munite), Lunit);
                           end if;

                           if Unreferenced_In_Spec (Item) then
                              Error_Msg_N
                                ("unit& is not referenced in spec?",
                                 Name (Item));

                           elsif No_Entities_Ref_In_Spec (Item) then
                              Error_Msg_N
                                ("no entities of & are referenced in spec?",
                                 Name (Item));

                           else
                              if Ekind (Ent) = E_Package then
                                 Check_Inner_Package (Ent);
                              end if;

                              exit;
                           end if;

                           if not Is_Visible_Renaming then
                              Error_Msg_N
                                ("\with clause might be moved to body?",
                                 Name (Item));
                           end if;

                           exit;

                        --  Move to next entity to continue search

                        else
                           Next_Entity (Ent);
                        end if;
                     end loop;
                  end if;

               --  For a generic package, the only interesting kind of
               --  reference is an instantiation, since entities cannot
               --  be referenced directly.

               elsif Is_Generic_Unit (Lunit) then

                  --  Unit was never instantiated, set flag for case of spec
                  --  call, or give warning for normal call.

                  if not Is_Instantiated (Lunit) then
                     if Unit = Spec_Unit then
                        Set_Unreferenced_In_Spec (Item);
                     else
                        Error_Msg_N
                          ("unit& is never instantiated?", Name (Item));
                     end if;

                  --  If unit was indeed instantiated, make sure that
                  --  flag is not set showing it was uninstantiated in
                  --  the spec, and if so, give warning.

                  elsif Unreferenced_In_Spec (Item) then
                     Error_Msg_N
                       ("unit& is not instantiated in spec?", Name (Item));
                     Error_Msg_N
                       ("\with clause can be moved to body?", Name (Item));
                  end if;
               end if;
            end if;

            Next (Item);
         end loop;

      end Check_One_Unit;

   --  Start of processing for Check_Unused_Withs

   begin
      if not Opt.Check_Withs
        or else Operating_Mode = Check_Syntax
      then
         return;
      end if;

      --  Flag any unused with clauses, but skip this step if we are
      --  compiling a subunit on its own, since we do not have enough
      --  information to determine whether with's are used. We will get
      --  the relevant warnings when we compile the parent. This is the
      --  normal style of GNAT compilation in any case.

      if Nkind (Unit (Cunit (Main_Unit))) = N_Subunit then
         return;
      end if;

      --  Process specified units

      if Spec_Unit = No_Unit then

         --  For main call, check all units

         for Unit in Main_Unit .. Last_Unit loop
            Check_One_Unit (Unit);
         end loop;

      else
         --  For call for spec, check only the spec

         Check_One_Unit (Spec_Unit);
      end if;
   end Check_Unused_Withs;

   ---------------------------------
   -- Generic_Package_Spec_Entity --
   ---------------------------------

   function Generic_Package_Spec_Entity (E : Entity_Id) return Boolean is
      S : Entity_Id;

   begin
      if Is_Package_Body_Entity (E) then
         return False;

      else
         S := Scope (E);

         loop
            if S = Standard_Standard then
               return False;

            elsif Ekind (S) = E_Generic_Package then
               return True;

            elsif Ekind (S) = E_Package then
               S := Scope (S);

            else
               return False;
            end if;
         end loop;
      end if;
   end Generic_Package_Spec_Entity;

   -------------------------------------
   -- Operand_Has_Warnings_Suppressed --
   -------------------------------------

   function Operand_Has_Warnings_Suppressed (N : Node_Id) return Boolean is

      function Check_For_Warnings (N : Node_Id) return Traverse_Result;
      --  Function used to check one node to see if it is or was originally
      --  a reference to an entity for which Warnings are off. If so, Abandon
      --  is returned, otherwise OK_Orig is returned to continue the traversal
      --  of the original expression.

      function Traverse is new Traverse_Func (Check_For_Warnings);
      --  Function used to traverse tree looking for warnings

      ------------------------
      -- Check_For_Warnings --
      ------------------------

      function Check_For_Warnings (N : Node_Id) return Traverse_Result is
         R : constant Node_Id := Original_Node (N);

      begin
         if Nkind (R) in N_Has_Entity
           and then Present (Entity (R))
           and then Warnings_Off (Entity (R))
         then
            return Abandon;
         else
            return OK_Orig;
         end if;
      end Check_For_Warnings;

   --  Start of processing for Operand_Has_Warnings_Suppressed

   begin
      return Traverse (N) = Abandon;

   --  If any exception occurs, then something has gone wrong, and this is
   --  only a minor aesthetic issue anyway, so just say we did not find what
   --  we are looking for, rather than blow up.

   exception
      when others =>
         return False;
   end Operand_Has_Warnings_Suppressed;

   ----------------------------------
   -- Output_Unreferenced_Messages --
   ----------------------------------

   procedure Output_Unreferenced_Messages is
      E : Entity_Id;

   begin
      for J in Unreferenced_Entities.First ..
               Unreferenced_Entities.Last
      loop
         E := Unreferenced_Entities.Table (J);

         if not Referenced (E) and then not Warnings_Off (E) then
            case Ekind (E) is
               when E_Variable =>

                  --  Case of variable that is assigned but not read. We
                  --  suppress the message if the variable is volatile,
                  --  has an address clause, or is imported.

                  if Referenced_As_LHS (E)
                    and then No (Address_Clause (E))
                    and then not Is_Volatile (E)
                  then
                     if Warn_On_Modified_Unread
                       and then not Is_Imported (E)
                     then
                        Error_Msg_N
                          ("variable & is assigned but never read?", E);
                     end if;

                  --  Normal case of neither assigned nor read

                  else
                     if Present (Renamed_Object (E))
                       and then Comes_From_Source (Renamed_Object (E))
                     then
                        Error_Msg_N
                          ("renamed variable & is not referenced?", E);
                     else
                        Error_Msg_N
                          ("variable & is not referenced?", E);
                     end if;
                  end if;

               when E_Constant =>
                  if Present (Renamed_Object (E))
                    and then Comes_From_Source (Renamed_Object (E))
                  then
                     Error_Msg_N ("renamed constant & is not referenced?", E);
                  else
                     Error_Msg_N ("constant & is not referenced?", E);
                  end if;

               when E_In_Parameter     |
                    E_Out_Parameter    |
                    E_In_Out_Parameter =>

                  --  Do not emit message for formals of a renaming, because
                  --  they are never referenced explicitly.

                  if Nkind (Original_Node (Unit_Declaration_Node (Scope (E))))
                    /= N_Subprogram_Renaming_Declaration
                  then
                     Error_Msg_N ("formal parameter & is not referenced?", E);
                  end if;

               when E_Named_Integer    |
                    E_Named_Real       =>
                  Error_Msg_N ("named number & is not referenced?", E);

               when E_Enumeration_Literal =>
                  Error_Msg_N ("literal & is not referenced?", E);

               when E_Function         =>
                  Error_Msg_N ("function & is not referenced?", E);

               when E_Procedure         =>
                  Error_Msg_N ("procedure & is not referenced?", E);

               when E_Generic_Procedure =>
                  Error_Msg_N
                    ("generic procedure & is never instantiated?", E);

               when E_Generic_Function  =>
                  Error_Msg_N ("generic function & is never instantiated?", E);

               when Type_Kind          =>
                  Error_Msg_N ("type & is not referenced?", E);

               when others =>
                  Error_Msg_N ("& is not referenced?", E);
            end case;

            Set_Warnings_Off (E);
         end if;
      end loop;
   end Output_Unreferenced_Messages;

   -----------------------------
   -- Warn_On_Known_Condition --
   -----------------------------

   procedure Warn_On_Known_Condition (C : Node_Id) is
      P : Node_Id;

   begin
      --   Argument replacement in an inlined body can make conditions
      --   static. Do not emit warnings in this case.

      if In_Inlined_Body then
         return;
      end if;

      if Constant_Condition_Warnings
        and then Nkind (C) = N_Identifier
        and then
          (Entity (C) = Standard_False or else Entity (C) = Standard_True)
        and then Comes_From_Source (Original_Node (C))
        and then not In_Instance
      then
         --  See if this is in a statement or a declaration

         P := Parent (C);
         loop
            --  If tree is not attached, do not issue warning (this is very
            --  peculiar, and probably arises from some other error condition)

            if No (P) then
               return;

            --  If we are in a declaration, then no warning, since in practice
            --  conditionals in declarations are used for intended tests which
            --  may be known at compile time, e.g. things like

            --    x : constant Integer := 2 + (Word'Size = 32);

            --  And a warning is annoying in such cases

            elsif Nkind (P) in N_Declaration
                    or else
                  Nkind (P) in N_Later_Decl_Item
            then
               return;

            --  Don't warn in assert pragma, since presumably tests in such
            --  a context are very definitely intended, and might well be
            --  known at compile time. Note that we have to test the original
            --  node, since assert pragmas get rewritten at analysis time.

            elsif Nkind (Original_Node (P)) = N_Pragma
              and then Chars (Original_Node (P)) = Name_Assert
            then
               return;
            end if;

            exit when Is_Statement (P);
            P := Parent (P);
         end loop;

         --  Here we issue the warning unless some sub-operand has warnings
         --  set off, in which case we suppress the warning for the node.

         if not Operand_Has_Warnings_Suppressed (C) then
            if Entity (C) = Standard_True then
               Error_Msg_N ("condition is always True?", C);
            else
               Error_Msg_N ("condition is always False?", C);
            end if;
         end if;
      end if;
   end Warn_On_Known_Condition;

end Sem_Warn;
