------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             R E S T R I C T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Casing;   use Casing;
with Errout;   use Errout;
with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with Lib;      use Lib;
with Opt;      use Opt;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Uname;    use Uname;

package body Restrict is

   Restricted_Profile_Result : Boolean := False;
   --  This switch memoizes the result of Restricted_Profile function
   --  calls for improved efficiency. Its setting is valid only if
   --  Restricted_Profile_Cached is True. Note that if this switch
   --  is ever set True, it need never be turned off again.

   Restricted_Profile_Cached : Boolean := False;
   --  This flag is set to True if the Restricted_Profile_Result
   --  contains the correct cached result of Restricted_Profile calls.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Restriction_Msg (Msg : String; R : String; N : Node_Id);
   --  Output error message at node N with given text, replacing the
   --  '%' in the message with the name of the restriction given as R,
   --  cased according to the current identifier casing. We do not use
   --  the normal insertion mechanism, since this requires an entry
   --  in the Names table, and this table will be locked if we are
   --  generating a message from gigi.

   function Same_Unit (U1, U2 : Node_Id) return Boolean;
   --  Returns True iff U1 and U2 represent the same library unit. Used for
   --  handling of No_Dependence => Unit restriction case.

   function Suppress_Restriction_Message (N : Node_Id) return Boolean;
   --  N is the node for a possible restriction violation message, but
   --  the message is to be suppressed if this is an internal file and
   --  this file is not the main unit.

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

   -------------------------
   -- Check_Compiler_Unit --
   -------------------------

   procedure Check_Compiler_Unit (N : Node_Id) is
   begin
      if Is_Compiler_Unit (Get_Source_Unit (N)) then
         Error_Msg_N ("use of construct not allowed in compiler", N);
      end if;
   end Check_Compiler_Unit;

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

   ----------------------------------
   -- Check_No_Implicit_Heap_Alloc --
   ----------------------------------

   procedure Check_No_Implicit_Heap_Alloc (N : Node_Id) is
   begin
      Check_Restriction (No_Implicit_Heap_Allocations, N);
   end Check_No_Implicit_Heap_Alloc;

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
                       Name_Buffer (Name_Len - 4 .. Name_Len) /= ".adb")
            then
               return;
            end if;

            --  Strip extension and pad to eight characters

            Name_Len := Name_Len - 4;
            while Name_Len < 8 loop
               Name_Len := Name_Len + 1;
               Name_Buffer (Name_Len) := ' ';
            end loop;

            --  If predefined unit, check the list of restricted units

            if Is_Predefined_File_Name (Fnam) then
               for J in Unit_Array'Range loop
                  if Name_Len = 8
                    and then Name_Buffer (1 .. 8) = Unit_Array (J).Filenm
                  then
                     Check_Restriction (Unit_Array (J).Res_Id, N);
                  end if;
               end loop;

               --  If not predefied unit, then one special check still remains.
               --  GNAT.Current_Exception is not allowed if we have restriction
               --  No_Exception_Propagation active.

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
      Rimage : constant String := Restriction_Id'Image (R);

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

            --  If checked by maximization, do maximization

            elsif R in Checked_Max_Parameter_Restrictions then
               Info.Count (R) := Integer'Max (Info.Count (R), VV);

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

      --  If in main extended unit, update main restrictions as well

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

      --  Here if restriction set, check for violation (either this is a
      --  Boolean restriction, or a parameter restriction with a value of
      --  zero and an unknown count, or a parameter restriction with a
      --  known value that exceeds the restriction count).

      elsif R in All_Boolean_Restrictions
        or else (Restrictions.Unknown (R)
                   and then Restrictions.Value (R) = 0)
        or else Restrictions.Count (R) > Restrictions.Value (R)
      then
         Error_Msg_Sloc := Restrictions_Loc (R);

         --  If we have a location for the Restrictions pragma, output it

         if Error_Msg_Sloc > No_Location
           or else Error_Msg_Sloc = System_Location
         then
            if Restriction_Warnings (R) then
               Restriction_Msg ("|violation of restriction %#?", Rimage, N);
            else
               Restriction_Msg ("|violation of restriction %#", Rimage, N);
            end if;

         --  Otherwise we have the case of an implicit restriction
         --  (e.g. a restriction implicitly set by another pragma)

         else
            Restriction_Msg
              ("|violation of implicit restriction %", Rimage, N);
         end if;
      end if;
   end Check_Restriction;

   -------------------------------------
   -- Check_Restriction_No_Dependence --
   -------------------------------------

   procedure Check_Restriction_No_Dependence (U : Node_Id; Err : Node_Id) is
      DU : Node_Id;

   begin
      for J in No_Dependence.First .. No_Dependence.Last loop
         DU := No_Dependence.Table (J).Unit;

         if Same_Unit (U, DU) then
            Error_Msg_Sloc := Sloc (DU);
            Error_Msg_Node_1 := DU;

            if No_Dependence.Table (J).Warn then
               Error_Msg
                 ("?violation of restriction `No_Dependence '='> &`#",
                  Sloc (Err));
            else
               Error_Msg
                 ("|violation of restriction `No_Dependence '='> &`#",
                  Sloc (Err));
            end if;

            return;
         end if;
      end loop;
   end Check_Restriction_No_Dependence;

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
         Restrictions.Set (J) := False;
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
            if S = Name_Buffer (1 .. Name_Len) then
               return J;
            end if;
         end;
      end loop;

      return Not_A_Restriction_Id;
   end Get_Restriction_Id;

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

   ----------------------------------
   -- Process_Restriction_Synonyms --
   ----------------------------------

   --  Note: body of this function must be coordinated with list of
   --  renaming declarations in System.Rident.

   function Process_Restriction_Synonyms (N : Node_Id) return Name_Id
   is
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

      if Warn_On_Obsolescent_Feature then
         Error_Msg_Name_1 := Old_Name;
         Error_Msg_N ("restriction identifier % is obsolescent?", N);
         Error_Msg_Name_1 := New_Name;
         Error_Msg_N ("|use restriction identifier % instead", N);
      end if;

      return New_Name;
   end Process_Restriction_Synonyms;

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
            R : Restriction_Flags  renames Profile_Info (Restricted).Set;
            V : Restriction_Values renames Profile_Info (Restricted).Value;
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
      return Restrictions.Set (R) and then not Restriction_Warnings (R);
   end Restriction_Active;

   ---------------------
   -- Restriction_Msg --
   ---------------------

   procedure Restriction_Msg (Msg : String; R : String; N : Node_Id) is
      B : String (1 .. Msg'Length + 2 * R'Length + 1);
      P : Natural := 1;

   begin
      Name_Buffer (1 .. R'Last) := R;
      Name_Len := R'Length;
      Set_Casing (Identifier_Casing (Get_Source_File_Index (Sloc (N))));

      P := 0;
      for J in Msg'Range loop
         if Msg (J) = '%' then
            P := P + 1;
            B (P) := '`';

            --  Put characters of image in message, quoting upper case letters

            for J in 1 .. Name_Len loop
               if Name_Buffer (J) in 'A' .. 'Z' then
                  P := P + 1;
                  B (P) := ''';
               end if;

               P := P + 1;
               B (P) := Name_Buffer (J);
            end loop;

            P := P + 1;
            B (P) := '`';

         else
            P := P + 1;
            B (P) := Msg (J);
         end if;
      end loop;

      Error_Msg_N (B (1 .. P), N);
   end Restriction_Msg;

   ---------------
   -- Same_Unit --
   ---------------

   function Same_Unit (U1, U2 : Node_Id) return Boolean is
   begin
      if Nkind (U1) = N_Identifier then
         return Nkind (U2) = N_Identifier and then Chars (U1) = Chars (U2);

      elsif Nkind (U2) = N_Identifier then
         return False;

      elsif (Nkind (U1) = N_Selected_Component
             or else Nkind (U1) = N_Expanded_Name)
        and then
          (Nkind (U2) = N_Selected_Component
           or else Nkind (U2) = N_Expanded_Name)
      then
         return Same_Unit (Prefix (U1), Prefix (U2))
           and then Same_Unit (Selector_Name (U1), Selector_Name (U2));
      else
         return False;
      end if;
   end Same_Unit;

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

   --  Case of Boolean restriction

   procedure Set_Restriction
     (R : All_Boolean_Restrictions;
      N : Node_Id)
   is
   begin
      --  Restriction No_Elaboration_Code must be enforced on a unit by unit
      --  basis. Hence, we avoid setting the restriction when processing an
      --  unit which is not the main one being compiled (or its corresponding
      --  spec). It can happen, for example, when processing an inlined body
      --  (the package containing the inlined subprogram is analyzed,
      --  including its pragma Restrictions).

      --  This seems like a very nasty kludge??? This is not the only per unit
      --  restriction why is this treated specially ???

      if R = No_Elaboration_Code
        and then Current_Sem_Unit /= Main_Unit
        and then Cunit (Current_Sem_Unit) /= Library_Unit (Cunit (Main_Unit))
      then
         return;
      end if;

      Restrictions.Set (R) := True;

      if Restricted_Profile_Cached and Restricted_Profile_Result then
         null;
      else
         Restricted_Profile_Cached := False;
      end if;

      --  Set location, but preserve location of system
      --  restriction for nice error msg with run time name

      if Restrictions_Loc (R) /= System_Location then
         Restrictions_Loc (R) := Sloc (N);
      end if;

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

   --  Case of parameter restriction

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

      --  Record the restriction if we are in the main unit,
      --  or in the extended main unit. The reason that we
      --  test separately for Main_Unit is that gnat.adc is
      --  processed with Current_Sem_Unit = Main_Unit, but
      --  nodes in gnat.adc do not appear to be the extended
      --  main source unit (they probably should do ???)

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
   end Set_Restriction;

   -----------------------------------
   -- Set_Restriction_No_Dependence --
   -----------------------------------

   procedure Set_Restriction_No_Dependence
     (Unit : Node_Id;
      Warn : Boolean)
   is
   begin
      --  Loop to check for duplicate entry

      for J in No_Dependence.First .. No_Dependence.Last loop

         --  Case of entry already in table

         if Same_Unit (Unit, No_Dependence.Table (J).Unit) then

            --  Error has precedence over warning

            if not Warn then
               No_Dependence.Table (J).Warn := False;
            end if;

            return;
         end if;
      end loop;

      --  Entry is not currently in table

      No_Dependence.Append ((Unit, Warn));
   end Set_Restriction_No_Dependence;

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
         return Is_Internal_File_Name (Unit_File_Name (Get_Source_Unit (N)));
      end if;
   end Suppress_Restriction_Message;

   ---------------------
   -- Tasking_Allowed --
   ---------------------

   function Tasking_Allowed return Boolean is
   begin
      return not Restrictions.Set (No_Tasking)
        and then (not Restrictions.Set (Max_Tasks)
                    or else Restrictions.Value (Max_Tasks) > 0);
   end Tasking_Allowed;

end Restrict;
