------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             R E S T R I C T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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
with Casing;         use Casing;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Errout;         use Errout;
with Debug;          use Debug;
with Fname;          use Fname;
with Fname.UF;       use Fname.UF;
with Lib;            use Lib;
with Opt;            use Opt;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Sinput;         use Sinput;
with Stand;          use Stand;
with Targparm;       use Targparm;
with Uname;          use Uname;
with Warnsw;         use Warnsw;

package body Restrict is

   --------------------------------
   -- Package Local Declarations --
   --------------------------------

   Config_Cunit_Boolean_Restrictions : Save_Cunit_Boolean_Restrictions;
   --  Save compilation unit restrictions set by config pragma files

   Global_Restriction_No_Tasking : Boolean := False;
   --  Set to True when No_Tasking is set in the run-time package System
   --  or in a configuration pragmas file (for example, gnat.adc).

   Restricted_Profile_Result : Boolean := False;
   --  This switch memoizes the result of Restricted_Profile function calls for
   --  improved efficiency. Valid only if Restricted_Profile_Cached is True.
   --  Note: if this switch is ever set True, it is never turned off again.

   Restricted_Profile_Cached : Boolean := False;
   --  This flag is set to True if the Restricted_Profile_Result contains the
   --  correct cached result of Restricted_Profile calls.

   No_Specification_Of_Aspects : array (Aspect_Id) of Source_Ptr :=
                                   (others => No_Location);
   --  Entries in this array are set to point to a previously occurring pragma
   --  that activates a No_Specification_Of_Aspect check.

   No_Specification_Of_Aspect_Warning : array (Aspect_Id) of Boolean :=
                                          (others => True);
   --  An entry in this array is set False in response to a previous call to
   --  Set_No_Specification_Of_Aspect for pragmas in the main unit that
   --  specify Warning as False. Once set False, an entry is never reset.

   No_Specification_Of_Aspect_Set : Boolean := False;
   --  Set True if any entry of No_Specification_Of_Aspects has been set True.
   --  Once set True, this is never turned off again.

   No_Use_Of_Attribute : array (Attribute_Id) of Source_Ptr :=
                           (others => No_Location);

   No_Use_Of_Attribute_Warning : array (Attribute_Id) of Boolean :=
                                   (others => False);

   No_Use_Of_Attribute_Set : Boolean := False;
   --  Indicates that No_Use_Of_Attribute was set at least once

   No_Use_Of_Pragma : array (Pragma_Id) of Source_Ptr :=
                        (others => No_Location);
   --  Source location of pragma No_Use_Of_Pragma for given pragma, a value
   --  of System_Location indicates occurrence in system.ads.

   No_Use_Of_Pragma_Warning : array (Pragma_Id) of Boolean :=
                                (others => False);

   No_Use_Of_Pragma_Set : Boolean := False;
   --  Indicates that No_Use_Of_Pragma was set at least once

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Restriction_Msg (R : Restriction_Id; N : Node_Id);
   --  Called if a violation of restriction R at node N is found. This routine
   --  outputs the appropriate message or messages taking care of warning vs
   --  real violation, serious vs non-serious, implicit vs explicit, the second
   --  message giving the profile name if needed, and the location information.

   function Same_Entity (E1, E2 : Node_Id) return Boolean;
   --  Returns True iff E1 and E2 represent the same entity. Used for handling
   --  of No_Use_Of_Entity => fully_qualified_ENTITY restriction case.

   function Same_Unit (U1, U2 : Node_Id) return Boolean;
   --  Returns True iff U1 and U2 represent the same library unit. Used for
   --  handling of No_Dependence => Unit restriction case.

   function Suppress_Restriction_Message (N : Node_Id) return Boolean;
   --  N is the node for a possible restriction violation message, but the
   --  message is to be suppressed if this is an internal file and this file is
   --  not the main unit. Returns True if message is to be suppressed.

   procedure Violation_Of_No_Dependence (Unit : Int; N : Node_Id);
   --  Called if a violation of restriction No_Dependence for Unit at node N
   --  is found. This routine outputs the appropriate message, taking care of
   --  warning vs real violation.

   -------------------
   -- Abort_Allowed --
   -------------------

   function Abort_Allowed return Boolean is
   begin
      if Restrictions.Set (No_Abort_Statements)
        and then Restrictions.Set (Max_Asynchronous_Select_Nesting)
        and then Restrictions.Value (Max_Asynchronous_Select_Nesting) = 0
      then
         return False;
      else
         return True;
      end if;
   end Abort_Allowed;

   ----------------------------------------
   -- Add_To_Config_Boolean_Restrictions --
   ----------------------------------------

   procedure Add_To_Config_Boolean_Restrictions (R : Restriction_Id) is
   begin
      Config_Cunit_Boolean_Restrictions (R) := True;
   end Add_To_Config_Boolean_Restrictions;
   --  Add specified restriction to stored configuration boolean restrictions.
   --  This is used for handling the special case of No_Elaboration_Code.

   ------------------------------------
   -- Check_Elaboration_Code_Allowed --
   ------------------------------------

   procedure Check_Elaboration_Code_Allowed (N : Node_Id) is
   begin
      Check_Restriction (No_Elaboration_Code, N);
   end Check_Elaboration_Code_Allowed;

   -----------------------------------------
   -- Check_Implicit_Dynamic_Code_Allowed --
   -----------------------------------------

   procedure Check_Implicit_Dynamic_Code_Allowed (N : Node_Id) is
   begin
      Check_Restriction (No_Implicit_Dynamic_Code, N);
   end Check_Implicit_Dynamic_Code_Allowed;

   --------------------------------
   -- Check_No_Implicit_Aliasing --
   --------------------------------

   procedure Check_No_Implicit_Aliasing (Obj : Node_Id) is
      E : Entity_Id;

   begin
      --  If restriction not active, nothing to check

      if not Restriction_Active (No_Implicit_Aliasing) then
         return;
      end if;

      --  If we have an entity name, check entity

      if Is_Entity_Name (Obj) then
         E := Entity (Obj);

         --  Restriction applies to entities that are objects

         if Is_Object (E) then
            if Is_Aliased (E) then
               return;

            elsif Present (Renamed_Object (E)) then
               Check_No_Implicit_Aliasing (Renamed_Object (E));
               return;
            end if;

         --  If we don't have an object, then it's OK

         else
            return;
         end if;

      --  For selected component, check selector

      elsif Nkind (Obj) = N_Selected_Component then
         Check_No_Implicit_Aliasing (Selector_Name (Obj));
         return;

      --  Indexed component is OK if aliased components

      elsif Nkind (Obj) = N_Indexed_Component then
         if Has_Aliased_Components (Etype (Prefix (Obj)))
           or else
             (Is_Access_Type (Etype (Prefix (Obj)))
               and then Has_Aliased_Components
                          (Designated_Type (Etype (Prefix (Obj)))))
         then
            return;
         end if;

      --  For type conversion, check converted expression

      elsif Nkind (Obj) in N_Unchecked_Type_Conversion | N_Type_Conversion then
         Check_No_Implicit_Aliasing (Expression (Obj));
         return;

      --  Explicit dereference is always OK

      elsif Nkind (Obj) = N_Explicit_Dereference then
         return;
      end if;

      --  If we fall through, then we have an aliased view that does not meet
      --  the rules for being explicitly aliased, so issue restriction msg.

      Check_Restriction (No_Implicit_Aliasing, Obj);
   end Check_No_Implicit_Aliasing;

   ----------------------------------
   -- Check_No_Implicit_Heap_Alloc --
   ----------------------------------

   procedure Check_No_Implicit_Heap_Alloc (N : Node_Id) is
   begin
      Check_Restriction (No_Implicit_Heap_Allocations, N);
   end Check_No_Implicit_Heap_Alloc;

   ----------------------------------
   -- Check_No_Implicit_Task_Alloc --
   ----------------------------------

   procedure Check_No_Implicit_Task_Alloc (N : Node_Id) is
   begin
      Check_Restriction (No_Implicit_Task_Allocations, N);
   end Check_No_Implicit_Task_Alloc;

   ---------------------------------------
   -- Check_No_Implicit_Protected_Alloc --
   ---------------------------------------

   procedure Check_No_Implicit_Protected_Alloc (N : Node_Id) is
   begin
      Check_Restriction (No_Implicit_Protected_Object_Allocations, N);
   end Check_No_Implicit_Protected_Alloc;

   -----------------------------------
   -- Check_Obsolescent_2005_Entity --
   -----------------------------------

   procedure Check_Obsolescent_2005_Entity (E : Entity_Id; N : Node_Id) is
      function Chars_Is (E : Entity_Id; S : String) return Boolean;
      --  Return True iff Chars (E) matches S (given in lower case)

      --------------
      -- Chars_Is --
      --------------

      function Chars_Is (E : Entity_Id; S : String) return Boolean is
         Nam : constant Name_Id := Chars (E);
      begin
         if Length_Of_Name (Nam) /= S'Length then
            return False;
         else
            return Get_Name_String (Nam) = S;
         end if;
      end Chars_Is;

   --  Start of processing for Check_Obsolescent_2005_Entity

   begin
      if Restriction_Check_Required (No_Obsolescent_Features)
        and then Ada_Version >= Ada_2005
        and then Chars_Is (Scope (E),                 "handling")
        and then Chars_Is (Scope (Scope (E)),         "characters")
        and then Chars_Is (Scope (Scope (Scope (E))), "ada")
        and then Scope (Scope (Scope (Scope (E)))) = Standard_Standard
      then
         if Chars_Is (E, "is_character")      or else
            Chars_Is (E, "is_string")         or else
            Chars_Is (E, "to_character")      or else
            Chars_Is (E, "to_string")         or else
            Chars_Is (E, "to_wide_character") or else
            Chars_Is (E, "to_wide_string")
         then
            Check_Restriction (No_Obsolescent_Features, N);
         end if;
      end if;
   end Check_Obsolescent_2005_Entity;

   ---------------------------
   -- Check_Restricted_Unit --
   ---------------------------

   procedure Check_Restricted_Unit (U : Unit_Name_Type; N : Node_Id) is
   begin
      if Suppress_Restriction_Message (N) then
         return;

      elsif Is_Spec_Name (U) then
         declare
            Fnam : constant File_Name_Type :=
                     Get_File_Name (U, Subunit => False);

         begin
            --  Get file name

            Get_Name_String (Fnam);

            --  Nothing to do if name not at least 5 characters long ending
            --  in .ads or .adb extension, which we strip.

            if Name_Len < 5
              or else (Name_Buffer (Name_Len - 3 .. Name_Len) /= ".ads"
                         and then
                       Name_Buffer (Name_Len - 3 .. Name_Len) /= ".adb")
            then
               return;
            end if;

            --  Strip extension and pad to eight characters

            Name_Len := Name_Len - 4;
            Add_Str_To_Name_Buffer ((Name_Len + 1 .. 8 => ' '));

            --  If predefined unit, check the list of restricted units

            if Is_Predefined_File_Name (Fnam) then
               for J in Unit_Array'Range loop
                  if Name_Len = 8
                    and then Name_Buffer (1 .. 8) = Unit_Array (J).Filenm
                  then
                     Check_Restriction (Unit_Array (J).Res_Id, N);
                  end if;
               end loop;

               --  If not predefined unit, then one special check still
               --  remains. GNAT.Current_Exception is not allowed if we have
               --  restriction No_Exception_Propagation active.

            else
               if Name_Buffer (1 .. 8) = "g-curexc" then
                  Check_Restriction (No_Exception_Propagation, N);
               end if;
            end if;
         end;
      end if;
   end Check_Restricted_Unit;

   -----------------------
   -- Check_Restriction --
   -----------------------

   procedure Check_Restriction
     (R : Restriction_Id;
      N : Node_Id;
      V : Uint := Uint_Minus_1)
   is
      Ignore_Msg_Issued : Boolean;
   begin
      Check_Restriction (Ignore_Msg_Issued, R, N, V);
   end Check_Restriction;

   procedure Check_Restriction
     (Msg_Issued : out Boolean;
      R          : Restriction_Id;
      N          : Node_Id;
      V          : Uint := Uint_Minus_1)
   is
      VV : Integer;
      --  V converted to integer form. If V is greater than Integer'Last,
      --  it is reset to minus 1 (unknown value).

      procedure Update_Restrictions (Info : in out Restrictions_Info);
      --  Update violation information in Info.Violated and Info.Count

      -------------------------
      -- Update_Restrictions --
      -------------------------

      procedure Update_Restrictions (Info : in out Restrictions_Info) is
      begin
         --  If not violated, set as violated now

         if not Info.Violated (R) then
            Info.Violated (R) := True;

            if R in All_Parameter_Restrictions then
               if VV < 0 then
                  Info.Unknown (R) := True;
                  Info.Count (R) := 1;

               else
                  Info.Count (R) := VV;
               end if;
            end if;

         --  Otherwise if violated already and a parameter restriction,
         --  update count by maximizing or summing depending on restriction.

         elsif R in All_Parameter_Restrictions then

            --  If new value is unknown, result is unknown

            if VV < 0 then
               Info.Unknown (R) := True;

            --  If checked by maximization, nothing to do because the
            --  check is per-object.

            elsif R in Checked_Max_Parameter_Restrictions then
               null;

            --  If checked by adding, do add, checking for overflow

            elsif R in Checked_Add_Parameter_Restrictions then
               declare
                  pragma Unsuppress (Overflow_Check);
               begin
                  Info.Count (R) := Info.Count (R) + VV;
               exception
                  when Constraint_Error =>
                     Info.Count (R) := Integer'Last;
                     Info.Unknown (R) := True;
               end;

            --  Should not be able to come here, known counts should only
            --  occur for restrictions that are Checked_max or Checked_Sum.

            else
               raise Program_Error;
            end if;
         end if;
      end Update_Restrictions;

   --  Start of processing for Check_Restriction

   begin
      Msg_Issued := False;

      --  In CodePeer mode, we do not want to check for any restriction, or set
      --  additional restrictions other than those already set in gnat1drv.adb
      --  so that we have consistency between each compilation.

      --  In GNATprove mode restrictions are checked, except for
      --  No_Initialize_Scalars, which is implicitly set in gnat1drv.adb.

      if CodePeer_Mode
        or else (GNATprove_Mode and then R = No_Initialize_Scalars)
      then
         return;
      end if;

      if UI_Is_In_Int_Range (V) then
         VV := Integer (UI_To_Int (V));
      else
         VV := -1;
      end if;

      --  Count can only be specified in the checked val parameter case

      pragma Assert (VV < 0 or else R in Checked_Val_Parameter_Restrictions);

      --  Nothing to do if value of zero specified for parameter restriction

      if VV = 0 then
         return;
      end if;

      --  Update current restrictions

      Update_Restrictions (Restrictions);

      --  If in main extended unit, update main restrictions as well. Note
      --  that as usual we check for Main_Unit explicitly to deal with the
      --  case of configuration pragma files.

      if Current_Sem_Unit = Main_Unit
        or else In_Extended_Main_Source_Unit (N)
      then
         Update_Restrictions (Main_Restrictions);
      end if;

      --  Nothing to do if restriction message suppressed

      if Suppress_Restriction_Message (N) then
         null;

      --  If restriction not set, nothing to do

      elsif not Restrictions.Set (R) then
         null;

      --  Don't complain about No_Obsolescent_Features in an instance, since we
      --  will complain on the template, which is much better. Are there other
      --  cases like this ??? Do we need a more general mechanism ???

      elsif R = No_Obsolescent_Features
        and then Instantiation_Location (Sloc (N)) /= No_Location
      then
         null;

      --  Here if restriction set, check for violation (this is a Boolean
      --  restriction, or a parameter restriction with a value of zero and an
      --  unknown count, or a parameter restriction with a known value that
      --  exceeds the restriction count).

      elsif R in All_Boolean_Restrictions
        or else (Restrictions.Unknown (R)
                   and then Restrictions.Value (R) = 0)
        or else Restrictions.Count (R) > Restrictions.Value (R)
      then
         Msg_Issued := True;
         Restriction_Msg (R, N);
      end if;

      --  For Max_Entries and the like, do not carry forward the violation
      --  count because it does not affect later declarations.

      if R in Checked_Max_Parameter_Restrictions then
         Restrictions.Count (R) := 0;
         Restrictions.Violated (R) := False;
      end if;
   end Check_Restriction;

   -------------------------------------
   -- Check_Restriction_No_Dependence --
   -------------------------------------

   procedure Check_Restriction_No_Dependence (U : Node_Id; Err : Node_Id) is
   begin
      --  Ignore call if node U is not in the main source unit. This avoids
      --  cascaded errors, e.g. when Ada.Containers units with other units.
      --  However, allow Standard_Location here, since this catches some cases
      --  of constructs that get converted to run-time calls.

      if not In_Extended_Main_Source_Unit (U)
        and then Sloc (U) /= Standard_Location
      then
         return;
      end if;

      --  Loop through entries in No_Dependence table to check each one in turn

      for J in No_Dependences.First .. No_Dependences.Last loop
         if Same_Unit (No_Dependences.Table (J).Unit, U) then
            Violation_Of_No_Dependence (J, Err);
            return;
         end if;
      end loop;
   end Check_Restriction_No_Dependence;

   -----------------------------------------------
   -- Check_Restriction_No_Dependence_On_System --
   -----------------------------------------------

   procedure Check_Restriction_No_Dependence_On_System
     (U   : Name_Id;
      Err : Node_Id)
   is
      pragma Assert (U /= No_Name);

   begin
      --  Loop through entries in No_Dependence table to check each one in turn

      for J in No_Dependences.First .. No_Dependences.Last loop
         if No_Dependences.Table (J).System_Child = U then
            Violation_Of_No_Dependence (J, Err);
            return;
         end if;
      end loop;
   end Check_Restriction_No_Dependence_On_System;

   --------------------------------------------------
   -- Check_Restriction_No_Specification_Of_Aspect --
   --------------------------------------------------

   procedure Check_Restriction_No_Specification_Of_Aspect (N : Node_Id) is
      A_Id : Aspect_Id;
      Id   : Node_Id;

   begin
      --  Ignore call if no instances of this restriction set

      if not No_Specification_Of_Aspect_Set then
         return;
      end if;

      --  Ignore call if node N is not in the main source unit, since we only
      --  give messages for the main unit. This avoids giving messages for
      --  aspects that are specified in withed units.

      if not In_Extended_Main_Source_Unit (N) then
         return;
      end if;

      if Nkind (N) = N_Pragma then
         Id := Pragma_Identifier (N);
      elsif Nkind (N) = N_Attribute_Definition_Clause then
         Id := N;
      else
         Id := Identifier (N);
      end if;

      A_Id := Get_Aspect_Id (Chars (Id));
      pragma Assert (A_Id /= No_Aspect);

      Error_Msg_Sloc := No_Specification_Of_Aspects (A_Id);

      if Error_Msg_Sloc /= No_Location then
         Error_Msg_Node_1 := Id;
         Error_Msg_Warn := No_Specification_Of_Aspect_Warning (A_Id);
         Error_Msg_N
           ("<*<violation of restriction `No_Specification_Of_Aspect '='> &`#",
            Id);
      end if;
   end Check_Restriction_No_Specification_Of_Aspect;

   -------------------------------------------
   -- Check_Restriction_No_Use_Of_Attribute --
   --------------------------------------------

   procedure Check_Restriction_No_Use_Of_Attribute (N : Node_Id) is
      Attr_Id  : Attribute_Id;
      Attr_Nam : Name_Id;

   begin
      --  Nothing to do if the attribute is not in the main source unit, since
      --  we only give messages for the main unit. This avoids giving messages
      --  for attributes that are specified in withed units.

      if not In_Extended_Main_Source_Unit (N) then
         return;

      --  Nothing to do if not checking No_Use_Of_Attribute

      elsif not No_Use_Of_Attribute_Set then
         return;

      --  Do not consider internally generated attributes because this leads to
      --  bizarre errors.

      elsif not Comes_From_Source (N) then
         return;
      end if;

      if Nkind (N) = N_Attribute_Definition_Clause then
         Attr_Nam := Chars (N);
      else
         pragma Assert (Nkind (N) = N_Attribute_Reference);
         Attr_Nam := Attribute_Name (N);
      end if;

      Attr_Id        := Get_Attribute_Id (Attr_Nam);
      Error_Msg_Sloc := No_Use_Of_Attribute (Attr_Id);

      if Error_Msg_Sloc /= No_Location then
         Error_Msg_Name_1 := Attr_Nam;
         Error_Msg_Warn   := No_Use_Of_Attribute_Warning (Attr_Id);
         Error_Msg_N
           ("<*<violation of restriction `No_Use_Of_Attribute '='> %` #", N);
      end if;
   end Check_Restriction_No_Use_Of_Attribute;

   ----------------------------------------
   -- Check_Restriction_No_Use_Of_Entity --
   ----------------------------------------

   procedure Check_Restriction_No_Use_Of_Entity (N : Node_Id) is
   begin
      --  Error defence (not clearly necessary, but better safe)

      if No (Entity (N)) then
         return;
      end if;

      --  If simple name of entity not flagged with Boolean2 flag, then there
      --  cannot be a matching entry in the table, so skip the search.

      if Get_Name_Table_Boolean2 (Chars (Entity (N))) = False then
         return;
      end if;

      --  Restriction is only recognized within a configuration pragma file,
      --  or within a unit of the main extended program. Note: the test for
      --  Main_Unit is needed to properly include the case of configuration
      --  pragma files.

      if Current_Sem_Unit /= Main_Unit
        and then not In_Extended_Main_Source_Unit (N)
      then
         return;
      end if;

      --  Here we must search the table

      for J in No_Use_Of_Entity.First .. No_Use_Of_Entity.Last loop
         declare
            NE_Ent : NE_Entry renames No_Use_Of_Entity.Table (J);
            Ent    : Entity_Id;
            Expr   : Node_Id;

         begin
            Ent  := Entity (N);
            Expr := NE_Ent.Entity;
            loop
               --  Here if at outer level of entity name in reference (handle
               --  also the direct use of Text_IO in the pragma). For example:
               --  pragma Restrictions (No_Use_Of_Entity => Text_IO.Put);

               if Scope (Ent) = Standard_Standard
                 or else (Nkind (Expr) = N_Identifier
                           and then Chars (Ent) = Name_Text_IO
                           and then Chars (Scope (Ent)) = Name_Ada
                           and then Scope (Scope (Ent)) = Standard_Standard)
               then
                  if Nkind (Expr) in N_Identifier | N_Operator_Symbol
                    and then Chars (Ent) = Chars (Expr)
                  then
                     Error_Msg_Node_1 := N;
                     Error_Msg_Warn := NE_Ent.Warn;
                     Error_Msg_Sloc := Sloc (NE_Ent.Entity);
                     Error_Msg_N
                       ("<*<reference to & violates restriction "
                        & "No_Use_Of_Entity #", N);
                     return;

                  else
                     exit;
                  end if;

               --  Here if at outer level of entity name in table

               elsif Nkind (Expr) in N_Identifier | N_Operator_Symbol then
                  exit;

               --  Here if neither at the outer level

               else
                  pragma Assert (Nkind (Expr) = N_Selected_Component);
                  exit when Chars (Selector_Name (Expr)) /= Chars (Ent);
               end if;

               --  Move up a level

               loop
                  Ent := Scope (Ent);
                  exit when not Is_Internal_Name (Chars (Ent));
               end loop;

               Expr := Prefix (Expr);
            end loop;
         end;
      end loop;
   end Check_Restriction_No_Use_Of_Entity;

   ----------------------------------------
   -- Check_Restriction_No_Use_Of_Pragma --
   ----------------------------------------

   procedure Check_Restriction_No_Use_Of_Pragma (N : Node_Id) is
      Id   : constant Node_Id   := Pragma_Identifier (N);
      P_Id : constant Pragma_Id := Get_Pragma_Id (Chars (Id));

   begin
      --  Nothing to do if the pragma is not in the main source unit, since we
      --  only give messages for the main unit. This avoids giving messages for
      --  pragmas that are specified in withed units.

      if not In_Extended_Main_Source_Unit (N) then
         return;

      --  Nothing to do if not checking No_Use_Of_Pragma

      elsif not No_Use_Of_Pragma_Set then
         return;

      --  Do not consider internally generated pragmas because this leads to
      --  bizarre errors.

      elsif not Comes_From_Source (N) then
         return;
      end if;

      Error_Msg_Sloc := No_Use_Of_Pragma (P_Id);

      if Error_Msg_Sloc /= No_Location then
         Error_Msg_Warn := No_Use_Of_Pragma_Warning (P_Id);
         Error_Msg_N
           ("<*<violation of restriction `No_Use_Of_Pragma '='> &` #", Id);
      end if;
   end Check_Restriction_No_Use_Of_Pragma;

   --------------------------------------
   -- Check_Wide_Character_Restriction --
   --------------------------------------

   procedure Check_Wide_Character_Restriction (E : Entity_Id; N : Node_Id) is
   begin
      if Restriction_Check_Required (No_Wide_Characters)
        and then Comes_From_Source (N)
      then
         declare
            T : constant Entity_Id := Root_Type (E);
         begin
            if T = Standard_Wide_Character      or else
               T = Standard_Wide_String         or else
               T = Standard_Wide_Wide_Character or else
               T = Standard_Wide_Wide_String
            then
               Check_Restriction (No_Wide_Characters, N);
            end if;
         end;
      end if;
   end Check_Wide_Character_Restriction;

   ----------------------------------------
   -- Cunit_Boolean_Restrictions_Restore --
   ----------------------------------------

   procedure Cunit_Boolean_Restrictions_Restore
     (R : Save_Cunit_Boolean_Restrictions)
   is
   begin
      for J in Cunit_Boolean_Restrictions loop
         Restrictions.Set (J) := R (J);
      end loop;

      --  If No_Elaboration_Code set in configuration restrictions, and we
      --  in the main extended source, then set it here now. This is part of
      --  the special processing for No_Elaboration_Code.

      if In_Extended_Main_Source_Unit (Cunit_Entity (Current_Sem_Unit))
        and then Config_Cunit_Boolean_Restrictions (No_Elaboration_Code)
      then
         Restrictions.Set (No_Elaboration_Code) := True;
      end if;
   end Cunit_Boolean_Restrictions_Restore;

   -------------------------------------
   -- Cunit_Boolean_Restrictions_Save --
   -------------------------------------

   function Cunit_Boolean_Restrictions_Save
     return Save_Cunit_Boolean_Restrictions
   is
      R : Save_Cunit_Boolean_Restrictions;

   begin
      for J in Cunit_Boolean_Restrictions loop
         R (J) := Restrictions.Set (J);
      end loop;

      return R;
   end Cunit_Boolean_Restrictions_Save;

   ------------------------
   -- Get_Restriction_Id --
   ------------------------

   function Get_Restriction_Id
     (N : Name_Id) return Restriction_Id
   is
   begin
      Get_Name_String (N);
      Set_Casing (All_Upper_Case);

      for J in All_Restrictions loop
         declare
            S : constant String := Restriction_Id'Image (J);
         begin
            if S = Name_Buffer (1 .. Name_Len)
              --  users cannot name the N_T_H_Implicit restriction
              and then J /= No_Task_Hierarchy_Implicit
            then
               return J;
            end if;
         end;
      end loop;

      return Not_A_Restriction_Id;
   end Get_Restriction_Id;

   -----------------------
   -- Global_No_Tasking --
   -----------------------

   function Global_No_Tasking return Boolean is
   begin
      return Global_Restriction_No_Tasking
        or else Targparm.Restrictions_On_Target.Set (No_Tasking);
   end Global_No_Tasking;

   ---------------------------------------------
   -- No_Dynamic_Accessibility_Checks_Enabled --
   ---------------------------------------------

   function No_Dynamic_Accessibility_Checks_Enabled
     (N : Node_Id) return Boolean
   is
      pragma Unreferenced (N);
      --  N is currently unreferenced but present for debugging purposes and
      --  potential future use.

   begin
      return Restrictions.Set (No_Dynamic_Accessibility_Checks);
   end No_Dynamic_Accessibility_Checks_Enabled;

   -------------------------------
   -- No_Exception_Handlers_Set --
   -------------------------------

   function No_Exception_Handlers_Set return Boolean is
   begin
      return (No_Run_Time_Mode or else Configurable_Run_Time_Mode)
        and then (Restrictions.Set (No_Exception_Handlers)
                    or else
                  Restrictions.Set (No_Exception_Propagation));
   end No_Exception_Handlers_Set;

   -------------------------------------
   -- No_Exception_Propagation_Active --
   -------------------------------------

   function No_Exception_Propagation_Active return Boolean is
   begin
      return (No_Run_Time_Mode
               or else Configurable_Run_Time_Mode
               or else Debug_Flag_Dot_G)
        and then Restriction_Active (No_Exception_Propagation);
   end No_Exception_Propagation_Active;

   --------------------------------
   -- OK_No_Dependence_Unit_Name --
   --------------------------------

   function OK_No_Dependence_Unit_Name (N : Node_Id) return Boolean is
   begin
      if Nkind (N) = N_Selected_Component then
         return
           OK_No_Dependence_Unit_Name (Prefix (N))
             and then
           OK_No_Dependence_Unit_Name (Selector_Name (N));

      elsif Nkind (N) = N_Identifier then
         return True;

      else
         Error_Msg_N ("wrong form for unit name for No_Dependence", N);
         return False;
      end if;
   end OK_No_Dependence_Unit_Name;

   ------------------------------
   -- OK_No_Use_Of_Entity_Name --
   ------------------------------

   function OK_No_Use_Of_Entity_Name (N : Node_Id) return Boolean is
   begin
      if Nkind (N) = N_Selected_Component then
         return
           OK_No_Use_Of_Entity_Name (Prefix (N))
             and then
           OK_No_Use_Of_Entity_Name (Selector_Name (N));

      elsif Nkind (N) in N_Identifier | N_Operator_Symbol then
         return True;

      else
         Error_Msg_N ("wrong form for entity name for No_Use_Of_Entity", N);
         return False;
      end if;
   end OK_No_Use_Of_Entity_Name;

   ----------------------------------
   -- Process_Restriction_Synonyms --
   ----------------------------------

   --  Note: body of this function must be coordinated with list of renaming
   --  declarations in System.Rident.

   function Process_Restriction_Synonyms (N : Node_Id) return Name_Id is
      Old_Name : constant Name_Id := Chars (N);
      New_Name : Name_Id;

   begin
      case Old_Name is
         when Name_Boolean_Entry_Barriers =>
            New_Name := Name_Simple_Barriers;

         when Name_Max_Entry_Queue_Depth =>
            New_Name := Name_Max_Entry_Queue_Length;

         when Name_No_Dynamic_Interrupts =>
            New_Name := Name_No_Dynamic_Attachment;

         when Name_No_Requeue =>
            New_Name := Name_No_Requeue_Statements;

         when Name_No_Task_Attributes =>
            New_Name := Name_No_Task_Attributes_Package;

         when others =>
            return Old_Name;
      end case;

      --  Output warning if we are warning on obsolescent features.

      if Warn_On_Obsolescent_Feature then
         Error_Msg_Name_1 := Old_Name;
         Error_Msg_N ("restriction identifier % is obsolescent?j?", N);
         Error_Msg_Name_1 := New_Name;
         Error_Msg_N ("|use restriction identifier % instead?j?", N);
      end if;

      return New_Name;
   end Process_Restriction_Synonyms;

   --------------------------------------
   -- Reset_Cunit_Boolean_Restrictions --
   --------------------------------------

   procedure Reset_Cunit_Boolean_Restrictions is
   begin
      for J in Cunit_Boolean_Restrictions loop
         Restrictions.Set (J) := False;
      end loop;
   end Reset_Cunit_Boolean_Restrictions;

   -----------------------------------------------
   -- Restore_Config_Cunit_Boolean_Restrictions --
   -----------------------------------------------

   procedure Restore_Config_Cunit_Boolean_Restrictions is
   begin
      Cunit_Boolean_Restrictions_Restore (Config_Cunit_Boolean_Restrictions);
   end Restore_Config_Cunit_Boolean_Restrictions;

   ------------------------
   -- Restricted_Profile --
   ------------------------

   function Restricted_Profile return Boolean is
   begin
      if Restricted_Profile_Cached then
         return Restricted_Profile_Result;

      else
         Restricted_Profile_Result := True;
         Restricted_Profile_Cached := True;

         declare
            R : Restriction_Flags  renames
                  Profile_Info (Restricted_Tasking).Set;
            V : Restriction_Values renames
                  Profile_Info (Restricted_Tasking).Value;
         begin
            for J in R'Range loop
               if R (J)
                 and then (Restrictions.Set (J) = False
                            or else Restriction_Warnings (J)
                            or else
                              (J in All_Parameter_Restrictions
                                and then Restrictions.Value (J) > V (J)))
               then
                  Restricted_Profile_Result := False;
                  exit;
               end if;
            end loop;

            return Restricted_Profile_Result;
         end;
      end if;
   end Restricted_Profile;

   ------------------------
   -- Restriction_Active --
   ------------------------

   function Restriction_Active (R : All_Restrictions) return Boolean is
   begin
      if Restrictions.Set (R) and then not Restriction_Warnings (R) then
         return True;
      else
         return R = No_Task_Hierarchy
           and then Restriction_Active (No_Task_Hierarchy_Implicit);
      end if;
   end Restriction_Active;

   --------------------------------
   -- Restriction_Check_Required --
   --------------------------------

   function Restriction_Check_Required (R : All_Restrictions) return Boolean is
   begin
      return Restrictions.Set (R);
   end Restriction_Check_Required;

   ---------------------
   -- Restriction_Msg --
   ---------------------

   procedure Restriction_Msg (R : Restriction_Id; N : Node_Id) is
      Msg : String (1 .. 100);
      Len : Natural := 0;

      procedure Add_Char (C : Character);
      --  Append given character to Msg, bumping Len

      procedure Add_Str (S : String);
      --  Append given string to Msg, bumping Len appropriately

      procedure Id_Case (S : String; Quotes : Boolean := True);
      --  Given a string S, case it according to current identifier casing, and
      --  store in Error_Msg_String. Then append `~` to the message buffer
      --  to output the string unchanged surrounded in quotes. The quotes
      --  are suppressed if Quotes = False.

      --------------
      -- Add_Char --
      --------------

      procedure Add_Char (C : Character) is
      begin
         Len := Len + 1;
         Msg (Len) := C;
      end Add_Char;

      -------------
      -- Add_Str --
      -------------

      procedure Add_Str (S : String) is
      begin
         Msg (Len + 1 .. Len + S'Length) := S;
         Len := Len + S'Length;
      end Add_Str;

      -------------
      -- Id_Case --
      -------------

      procedure Id_Case (S : String; Quotes : Boolean := True) is
      begin
         Name_Buffer (1 .. S'Last) := S;
         Name_Len := S'Length;
         Set_Casing (Identifier_Casing (Get_Source_File_Index (Sloc (N))));
         Error_Msg_Strlen := Name_Len;
         Error_Msg_String (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);

         if Quotes then
            Add_Str ("`~`");
         else
            Add_Char ('~');
         end if;
      end Id_Case;

   --  Start of processing for Restriction_Msg

   begin
      --  Set warning message if warning

      if Restriction_Warnings (R) then
         Add_Str ("?*?");

      --  If real violation (not warning), then mark it as non-serious unless
      --  it is a violation of No_Finalization in which case we leave it as a
      --  serious message, since otherwise we get crashes during attempts to
      --  expand stuff that is not properly formed due to assumptions made
      --  about no finalization being present.

      elsif R /= No_Finalization then
         Add_Char ('|');
      end if;

      Error_Msg_Sloc := Restrictions_Loc (R);

      --  Set main message, adding implicit if no source location

      if Error_Msg_Sloc > No_Location
        or else Error_Msg_Sloc = System_Location
      then
         Add_Str ("violation of restriction ");
      else
         Add_Str ("violation of implicit restriction ");
         Error_Msg_Sloc := No_Location;
      end if;

      --  Case of parameterized restriction

      if R in All_Parameter_Restrictions then
         Add_Char ('`');
         Id_Case (Restriction_Id'Image (R), Quotes => False);
         Add_Str (" = ^`");
         Error_Msg_Uint_1 := UI_From_Int (Int (Restrictions.Value (R)));

      --  Case of boolean restriction

      else
         Id_Case (Restriction_Id'Image (R));
      end if;

      --  Case of no secondary profile continuation message

      if Restriction_Profile_Name (R) = No_Profile then
         if Error_Msg_Sloc /= No_Location then
            Add_Char ('#');
         end if;

         Add_Char ('!');
         Error_Msg_N (Msg (1 .. Len), N);

      --  Case of secondary profile continuation message present

      else
         Add_Char ('!');
         Error_Msg_N (Msg (1 .. Len), N);

         Len := 0;
         Add_Char ('\');

         --  Set as warning if warning case

         if Restriction_Warnings (R) then
            Add_Str ("??");
         end if;

         --  Set main message

         Add_Str ("from profile ");
         Id_Case (Profile_Name'Image (Restriction_Profile_Name (R)));

         --  Add location if we have one

         if Error_Msg_Sloc /= No_Location then
            Add_Char ('#');
         end if;

         --  Output unconditional message and we are done

         Add_Char ('!');
         Error_Msg_N (Msg (1 .. Len), N);
      end if;
   end Restriction_Msg;

   -----------------
   -- Same_Entity --
   -----------------

   function Same_Entity (E1, E2 : Node_Id) return Boolean is
   begin
      if Nkind (E1) in N_Identifier | N_Operator_Symbol
           and then
         Nkind (E2) in N_Identifier | N_Operator_Symbol
      then
         return Chars (E1) = Chars (E2);

      elsif Nkind (E1) in N_Selected_Component | N_Expanded_Name
              and then
            Nkind (E2) in N_Selected_Component | N_Expanded_Name
      then
         return Same_Unit (Prefix (E1), Prefix (E2))
                  and then
                Same_Unit (Selector_Name (E1), Selector_Name (E2));
      else
         return False;
      end if;
   end Same_Entity;

   ---------------
   -- Same_Unit --
   ---------------

   function Same_Unit (U1, U2 : Node_Id) return Boolean is
   begin
      if Nkind (U1) = N_Identifier and then Nkind (U2) = N_Identifier then
         return Chars (U1) = Chars (U2);

      elsif Nkind (U1) in N_Selected_Component | N_Expanded_Name
              and then
            Nkind (U2) in N_Selected_Component | N_Expanded_Name
      then
         return Same_Unit (Prefix (U1), Prefix (U2))
                  and then
                Same_Unit (Selector_Name (U1), Selector_Name (U2));
      else
         return False;
      end if;
   end Same_Unit;

   --------------------------------------------
   -- Save_Config_Cunit_Boolean_Restrictions --
   --------------------------------------------

   procedure Save_Config_Cunit_Boolean_Restrictions is
   begin
      Config_Cunit_Boolean_Restrictions := Cunit_Boolean_Restrictions_Save;
   end Save_Config_Cunit_Boolean_Restrictions;

   ------------------------------
   -- Set_Profile_Restrictions --
   ------------------------------

   procedure Set_Profile_Restrictions
     (P    : Profile_Name;
      N    : Node_Id;
      Warn : Boolean)
   is
      R : Restriction_Flags  renames Profile_Info (P).Set;
      V : Restriction_Values renames Profile_Info (P).Value;

   begin
      for J in R'Range loop
         if R (J) then
            declare
               Already_Restricted : constant Boolean := Restriction_Active (J);

            begin
               --  Set the restriction

               if J in All_Boolean_Restrictions then
                  Set_Restriction (J, N);
               else
                  Set_Restriction (J, N, V (J));
               end if;

               --  Record that this came from a Profile[_Warnings] restriction

               Restriction_Profile_Name (J) := P;

               --  Set warning flag, except that we do not set the warning
               --  flag if the restriction was already active and this is
               --  the warning case. That avoids a warning overriding a real
               --  restriction, which should never happen.

               if not (Warn and Already_Restricted) then
                  Restriction_Warnings (J) := Warn;
               end if;
            end;
         end if;
      end loop;
   end Set_Profile_Restrictions;

   ---------------------
   -- Set_Restriction --
   ---------------------

   procedure Set_Restriction
     (R : All_Boolean_Restrictions;
      N : Node_Id)
   is
   begin
      Restrictions.Set (R) := True;

      if Restricted_Profile_Cached and Restricted_Profile_Result then
         null;
      else
         Restricted_Profile_Cached := False;
      end if;

      --  Set location, but preserve location of system restriction for nice
      --  error msg with run time name.

      if Restrictions_Loc (R) /= System_Location then
         Restrictions_Loc (R) := Sloc (N);
      end if;

      --  Note restriction came from restriction pragma, not profile

      Restriction_Profile_Name (R) := No_Profile;

      --  Record the restriction if we are in the main unit, or in the extended
      --  main unit. The reason that we test separately for Main_Unit is that
      --  gnat.adc is processed with Current_Sem_Unit = Main_Unit, but nodes in
      --  gnat.adc do not appear to be in the extended main source unit (they
      --  probably should do ???)

      if Current_Sem_Unit = Main_Unit
        or else In_Extended_Main_Source_Unit (N)
      then
         if not Restriction_Warnings (R) then
            Main_Restrictions.Set (R) := True;
         end if;
      end if;
   end Set_Restriction;

   procedure Set_Restriction
     (R : All_Parameter_Restrictions;
      N : Node_Id;
      V : Integer)
   is
   begin
      if Restricted_Profile_Cached and Restricted_Profile_Result then
         null;
      else
         Restricted_Profile_Cached := False;
      end if;

      if Restrictions.Set (R) then
         if V < Restrictions.Value (R) then
            Restrictions.Value (R) := V;
            Restrictions_Loc (R) := Sloc (N);
         end if;

      else
         Restrictions.Set (R) := True;
         Restrictions.Value (R) := V;
         Restrictions_Loc (R) := Sloc (N);
      end if;

      --  Record the restriction if we are in the main unit, or in the extended
      --  main unit. The reason that we test separately for Main_Unit is that
      --  gnat.adc is processed with Current_Sem_Unit = Main_Unit, but nodes in
      --  gnat.adc do not appear to be the extended main source unit (they
      --  probably should do ???)

      if Current_Sem_Unit = Main_Unit
        or else In_Extended_Main_Source_Unit (N)
      then
         if Main_Restrictions.Set (R) then
            if V < Main_Restrictions.Value (R) then
               Main_Restrictions.Value (R) := V;
            end if;

         elsif not Restriction_Warnings (R) then
            Main_Restrictions.Set (R) := True;
            Main_Restrictions.Value (R) := V;
         end if;
      end if;

      --  Note restriction came from restriction pragma, not profile

      Restriction_Profile_Name (R) := No_Profile;
   end Set_Restriction;

   procedure Set_Restriction
     (R    : All_Restrictions;
      N    : Node_Id;
      Warn : Boolean;
      V    : Integer := Integer'First)
   is
      Set : Boolean := True;
   begin
      if Warn and then Restriction_Active (R) then
         Set := False;
      end if;

      if Set then
         if R in All_Boolean_Restrictions then
            Set_Restriction (R, N);
         else
            Set_Restriction (R, N, V);
         end if;

         Restriction_Warnings (R) := Warn;
      end if;
   end Set_Restriction;

   -----------------------------------
   -- Set_Restriction_No_Dependence --
   -----------------------------------

   procedure Set_Restriction_No_Dependence
     (Unit    : Node_Id;
      Warn    : Boolean;
      Profile : Profile_Name := No_Profile)
   is
      ND : ND_Entry;

   begin
      --  Loop to check for duplicate entry

      for J in No_Dependences.First .. No_Dependences.Last loop

         --  Case of entry already in table

         if Same_Unit (Unit, No_Dependences.Table (J).Unit) then

            --  Error has precedence over warning

            if not Warn then
               No_Dependences.Table (J).Warn := False;
            end if;

            return;
         end if;
      end loop;

      --  Entry is not currently in table

      ND := (Unit, No_Name, Warn, Profile);

      --  Check whether this is a child unit of System

      if Nkind (Unit) = N_Selected_Component then
         declare
            Root : Node_Id := Unit;

         begin
            while Nkind (Prefix (Root)) = N_Selected_Component loop
               Root := Prefix (Root);
            end loop;

            if Chars (Prefix (Root)) = Name_System then
               ND.System_Child := Chars (Selector_Name (Root));
            end if;
         end;
      end if;

      No_Dependences.Append (ND);
   end Set_Restriction_No_Dependence;

   --------------------------------------
   -- Set_Restriction_No_Use_Of_Entity --
   --------------------------------------

   procedure Set_Restriction_No_Use_Of_Entity
     (Entity  : Node_Id;
      Warn    : Boolean;
      Profile : Profile_Name := No_Profile)
   is
      Nam : Node_Id;

   begin
      --  Loop to check for duplicate entry

      for J in No_Use_Of_Entity.First .. No_Use_Of_Entity.Last loop

         --  Case of entry already in table

         if Same_Entity (Entity, No_Use_Of_Entity.Table (J).Entity) then

            --  Error has precedence over warning

            if not Warn then
               No_Use_Of_Entity.Table (J).Warn := False;
            end if;

            return;
         end if;
      end loop;

      --  Entry is not currently in table

      No_Use_Of_Entity.Append ((Entity, Warn, Profile));

      --  Now we need to find the direct name and set Boolean2 flag

      if Nkind (Entity) in N_Identifier | N_Operator_Symbol then
         Nam := Entity;

      else
         pragma Assert (Nkind (Entity) = N_Selected_Component);
         Nam := Selector_Name (Entity);
         pragma Assert (Nkind (Nam) in N_Identifier | N_Operator_Symbol);
      end if;

      Set_Name_Table_Boolean2 (Chars (Nam), True);
   end Set_Restriction_No_Use_Of_Entity;

   ------------------------------------------------
   -- Set_Restriction_No_Specification_Of_Aspect --
   ------------------------------------------------

   procedure Set_Restriction_No_Specification_Of_Aspect
     (N    : Node_Id;
      Warn : Boolean)
   is
      A_Id : constant Aspect_Id_Exclude_No_Aspect := Get_Aspect_Id (Chars (N));

   begin
      No_Specification_Of_Aspect_Set := True;
      No_Specification_Of_Aspects (A_Id) := Sloc (N);
      No_Specification_Of_Aspect_Warning (A_Id) := Warn;
   end Set_Restriction_No_Specification_Of_Aspect;

   procedure Set_Restriction_No_Specification_Of_Aspect (A_Id : Aspect_Id) is
   begin
      No_Specification_Of_Aspect_Set := True;
      No_Specification_Of_Aspects (A_Id) := System_Location;
      No_Specification_Of_Aspect_Warning (A_Id) := False;
   end Set_Restriction_No_Specification_Of_Aspect;

   -----------------------------------------
   -- Set_Restriction_No_Use_Of_Attribute --
   -----------------------------------------

   procedure Set_Restriction_No_Use_Of_Attribute
     (N    : Node_Id;
      Warn : Boolean)
   is
      A_Id : constant Attribute_Id := Get_Attribute_Id (Chars (N));

   begin
      No_Use_Of_Attribute_Set := True;
      No_Use_Of_Attribute (A_Id) := Sloc (N);
      No_Use_Of_Attribute_Warning (A_Id) := Warn;
   end Set_Restriction_No_Use_Of_Attribute;

   procedure Set_Restriction_No_Use_Of_Attribute (A_Id : Attribute_Id) is
   begin
      No_Use_Of_Attribute_Set := True;
      No_Use_Of_Attribute (A_Id) := System_Location;
      No_Use_Of_Attribute_Warning (A_Id) := False;
   end Set_Restriction_No_Use_Of_Attribute;

   --------------------------------------
   -- Set_Restriction_No_Use_Of_Pragma --
   --------------------------------------

   procedure Set_Restriction_No_Use_Of_Pragma
     (N    : Node_Id;
      Warn : Boolean)
   is
      A_Id : constant Pragma_Id := Get_Pragma_Id (Chars (N));

   begin
      No_Use_Of_Pragma_Set := True;
      No_Use_Of_Pragma (A_Id) := Sloc (N);
      No_Use_Of_Pragma_Warning (A_Id) := Warn;
   end Set_Restriction_No_Use_Of_Pragma;

   procedure Set_Restriction_No_Use_Of_Pragma (A_Id : Pragma_Id) is
   begin
      No_Use_Of_Pragma_Set := True;
      No_Use_Of_Pragma (A_Id) := System_Location;
      No_Use_Of_Pragma_Warning (A_Id) := False;
   end Set_Restriction_No_Use_Of_Pragma;

   ---------------------------
   -- Set_Global_No_Tasking --
   ---------------------------

   procedure Set_Global_No_Tasking is
   begin
      Global_Restriction_No_Tasking := True;
   end Set_Global_No_Tasking;

   ----------------------------------
   -- Suppress_Restriction_Message --
   ----------------------------------

   function Suppress_Restriction_Message (N : Node_Id) return Boolean is
   begin
      --  We only output messages for the extended main source unit

      if In_Extended_Main_Source_Unit (N) then
         return False;

      --  If loaded by rtsfind, then suppress message

      elsif Sloc (N) <= No_Location then
         return True;

      --  Otherwise suppress message if internal file

      else
         return In_Internal_Unit (N);
      end if;
   end Suppress_Restriction_Message;

   --------------------------------
   -- Violation_Of_No_Dependence --
   --------------------------------

   procedure Violation_Of_No_Dependence (Unit : Int; N : Node_Id) is
   begin
      Error_Msg_Node_1 := No_Dependences.Table (Unit).Unit;
      Error_Msg_Sloc   := Sloc (Error_Msg_Node_1);

      if No_Dependences.Table (Unit).Warn then
         Error_Msg
           ("?*?violation of restriction `No_Dependence '='> &`#", Sloc (N));
      else
         Error_Msg
           ("|violation of restriction `No_Dependence '='> &`#", Sloc (N));
      end if;
   end Violation_Of_No_Dependence;

   ---------------------
   -- Tasking_Allowed --
   ---------------------

   function Tasking_Allowed return Boolean is
   begin
      return not Restrictions.Set (No_Tasking)
        and then (not Restrictions.Set (Max_Tasks)
                   or else Restrictions.Value (Max_Tasks) > 0)
        and then not No_Run_Time_Mode;
   end Tasking_Allowed;

end Restrict;
