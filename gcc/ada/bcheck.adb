------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               B C H E C K                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

with ALI;      use ALI;
with ALI.Util; use ALI.Util;
with Binderr;  use Binderr;
with Butil;    use Butil;
with Casing;   use Casing;
with Fname;    use Fname;
with Gnatvsn;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;
with Output;   use Output;
with Rident;   use Rident;
with Types;    use Types;

package body Bcheck is

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  The following checking subprograms make up the parts of the
   --  configuration consistency check. See bodies for details of checks.

   procedure Check_Consistent_Dispatching_Policy;
   procedure Check_Consistent_Dynamic_Elaboration_Checking;
   procedure Check_Consistent_Interrupt_States;
   procedure Check_Consistent_Locking_Policy;
   procedure Check_Consistent_No_Component_Reordering;
   procedure Check_Consistent_Normalize_Scalars;
   procedure Check_Consistent_Optimize_Alignment;
   procedure Check_Consistent_Partition_Elaboration_Policy;
   procedure Check_Consistent_Queuing_Policy;
   procedure Check_Consistent_Restrictions;
   procedure Check_Consistent_Restriction_No_Default_Initialization;
   procedure Check_Consistent_SSO_Default;
   procedure Check_Consistent_Exception_Handling;

   procedure Consistency_Error_Msg (Msg : String);
   --  Produce an error or a warning message, depending on whether an
   --  inconsistent configuration is permitted or not.

   function Same_Unit (U1 : Unit_Name_Type; U2 : Name_Id) return Boolean;
   --  Used to compare two unit names for No_Dependence checks. U1 is in
   --  standard unit name format, and U2 is in literal form with periods.

   -------------------------------------
   -- Check_Configuration_Consistency --
   -------------------------------------

   procedure Check_Configuration_Consistency is
   begin
      if Queuing_Policy_Specified /= ' ' then
         Check_Consistent_Queuing_Policy;
      end if;

      if Locking_Policy_Specified /= ' ' then
         Check_Consistent_Locking_Policy;
      end if;

      if No_Component_Reordering_Specified then
         Check_Consistent_No_Component_Reordering;
      end if;

      if Partition_Elaboration_Policy_Specified /= ' ' then
         Check_Consistent_Partition_Elaboration_Policy;
      end if;

      if SSO_Default_Specified then
         Check_Consistent_SSO_Default;
      end if;

      if Zero_Cost_Exceptions_Specified
        or else Frontend_Exceptions_Specified
      then
         Check_Consistent_Exception_Handling;
      end if;

      Check_Consistent_Normalize_Scalars;
      Check_Consistent_Optimize_Alignment;
      Check_Consistent_Dynamic_Elaboration_Checking;
      Check_Consistent_Restrictions;
      Check_Consistent_Restriction_No_Default_Initialization;
      Check_Consistent_Interrupt_States;
      Check_Consistent_Dispatching_Policy;
   end Check_Configuration_Consistency;

   -----------------------
   -- Check_Consistency --
   -----------------------

   procedure Check_Consistency is
      Src : Source_Id;
      --  Source file Id for this Sdep entry

      ALI_Path_Id : File_Name_Type;

   begin
      --  First, we go through the source table to see if there are any cases
      --  in which we should go after source files and compute checksums of
      --  the source files. We need to do this for any file for which we have
      --  mismatching time stamps and (so far) matching checksums.

      for S in Source.First .. Source.Last loop

         --  If all time stamps for a file match, then there is nothing to
         --  do, since we will not be checking checksums in that case anyway

         if Source.Table (S).All_Timestamps_Match then
            null;

         --  If we did not find the source file, then we can't compute its
         --  checksum anyway. Note that when we have a time stamp mismatch,
         --  we try to find the source file unconditionally (i.e. if
         --  Check_Source_Files is False).

         elsif not Source.Table (S).Source_Found then
            null;

         --  If we already have non-matching or missing checksums, then no
         --  need to try going after source file, since we won't trust the
         --  checksums in any case.

         elsif not Source.Table (S).All_Checksums_Match then
            null;

         --  Now we have the case where we have time stamp mismatches, and
         --  the source file is around, but so far all checksums match. This
         --  is the case where we need to compute the checksum from the source
         --  file, since otherwise we would ignore the time stamp mismatches,
         --  and that is wrong if the checksum of the source does not agree
         --  with the checksums in the ALI files.

         elsif Check_Source_Files then
            if not Checksums_Match
              (Source.Table (S).Checksum,
               Get_File_Checksum (Source.Table (S).Sfile))
            then
               Source.Table (S).All_Checksums_Match := False;
            end if;
         end if;
      end loop;

      --  Loop through ALI files

      ALIs_Loop : for A in ALIs.First .. ALIs.Last loop

         --  Loop through Sdep entries in one ALI file

         Sdep_Loop : for D in
           ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep
         loop
            if Sdep.Table (D).Dummy_Entry then
               goto Continue;
            end if;

            Src := Source_Id (Get_Name_Table_Int (Sdep.Table (D).Sfile));

            --  If the time stamps match, or all checksums match, then we
            --  are OK, otherwise we have a definite error.

            if Sdep.Table (D).Stamp /= Source.Table (Src).Stamp
              and then not Source.Table (Src).All_Checksums_Match
            then
               Error_Msg_File_1 := ALIs.Table (A).Sfile;
               Error_Msg_File_2 := Sdep.Table (D).Sfile;

               --  Two styles of message, depending on whether or not
               --  the updated file is the one that must be recompiled

               if Error_Msg_File_1 = Error_Msg_File_2 then
                  if Tolerate_Consistency_Errors then
                     Error_Msg
                        ("?{ has been modified and should be recompiled");
                  else
                     Error_Msg
                       ("{ has been modified and must be recompiled");
                  end if;

               else
                  ALI_Path_Id :=
                    Osint.Full_Lib_File_Name (ALIs.Table (A).Afile);

                  if Osint.Is_Readonly_Library (ALI_Path_Id) then
                     if Tolerate_Consistency_Errors then
                        Error_Msg ("?{ should be recompiled");
                        Error_Msg_File_1 := ALI_Path_Id;
                        Error_Msg ("?({ is obsolete and read-only)");
                     else
                        Error_Msg ("{ must be compiled");
                        Error_Msg_File_1 := ALI_Path_Id;
                        Error_Msg ("({ is obsolete and read-only)");
                     end if;

                  elsif Tolerate_Consistency_Errors then
                     Error_Msg
                       ("?{ should be recompiled ({ has been modified)");

                  else
                     Error_Msg ("{ must be recompiled ({ has been modified)");
                  end if;
               end if;

               if (not Tolerate_Consistency_Errors) and Verbose_Mode then
                  Error_Msg_File_1 := Source.Table (Src).Stamp_File;

                  if Source.Table (Src).Source_Found then
                     Error_Msg_File_1 :=
                       Osint.Full_Source_Name (Error_Msg_File_1);
                  else
                     Error_Msg_File_1 :=
                       Osint.Full_Lib_File_Name (Error_Msg_File_1);
                  end if;

                  Error_Msg
                    ("time stamp from { " & String (Source.Table (Src).Stamp));

                  Error_Msg_File_1 := Sdep.Table (D).Sfile;
                  Error_Msg
                    (" conflicts with { timestamp " &
                     String (Sdep.Table (D).Stamp));

                  Error_Msg_File_1 :=
                    Osint.Full_Lib_File_Name (ALIs.Table (A).Afile);
                  Error_Msg (" from {");
               end if;

               --  Exit from the loop through Sdep entries once we find one
               --  that does not match.

               exit Sdep_Loop;
            end if;

         <<Continue>>
            null;
         end loop Sdep_Loop;
      end loop ALIs_Loop;
   end Check_Consistency;

   -----------------------------------------
   -- Check_Consistent_Dispatching_Policy --
   -----------------------------------------

   --  The rule is that all files for which the dispatching policy is
   --  significant must meet the following rules:

   --    1. All files for which a task dispatching policy is significant must
   --    be compiled with the same setting.

   --    2. If a partition contains one or more Priority_Specific_Dispatching
   --    pragmas it cannot contain a Task_Dispatching_Policy pragma.

   --    3. No overlap is allowed in the priority ranges specified in
   --    Priority_Specific_Dispatching pragmas within the same partition.

   --    4. If a partition contains one or more Priority_Specific_Dispatching
   --    pragmas then the Ceiling_Locking policy is the only one allowed for
   --    the partition.

   procedure Check_Consistent_Dispatching_Policy is
      Max_Prio : Nat := 0;
      --  Maximum priority value for which a Priority_Specific_Dispatching
      --  pragma has been specified.

      TDP_Pragma_Afile : ALI_Id := No_ALI_Id;
      --  ALI file where a Task_Dispatching_Policy pragma appears

   begin
      --  Consistency checks in units specifying a Task_Dispatching_Policy

      if Task_Dispatching_Policy_Specified /= ' ' then
         Find_Policy : for A1 in ALIs.First .. ALIs.Last loop
            if ALIs.Table (A1).Task_Dispatching_Policy /= ' ' then

               --  Store the place where the first task dispatching pragma
               --  appears. We may need this value for issuing consistency
               --  errors if Priority_Specific_Dispatching pragmas are used.

               TDP_Pragma_Afile := A1;

               Check_Policy : declare
                  Policy : constant Character :=
                    ALIs.Table (A1).Task_Dispatching_Policy;

               begin
                  for A2 in A1 + 1 .. ALIs.Last loop
                     if ALIs.Table (A2).Task_Dispatching_Policy /= ' '
                          and then
                        ALIs.Table (A2).Task_Dispatching_Policy /= Policy
                     then
                        Error_Msg_File_1 := ALIs.Table (A1).Sfile;
                        Error_Msg_File_2 := ALIs.Table (A2).Sfile;

                        Consistency_Error_Msg
                          ("{ and { compiled with different task" &
                           " dispatching policies");
                        exit Find_Policy;
                     end if;
                  end loop;
               end Check_Policy;

               exit Find_Policy;
            end if;
         end loop Find_Policy;
      end if;

      --  If no Priority_Specific_Dispatching entries, nothing else to do

      if Specific_Dispatching.Last >= Specific_Dispatching.First then

         --  Find out the maximum priority value for which one of the
         --  Priority_Specific_Dispatching pragmas applies.

         Max_Prio := 0;
         for J in Specific_Dispatching.First .. Specific_Dispatching.Last loop
            if Specific_Dispatching.Table (J).Last_Priority > Max_Prio then
               Max_Prio := Specific_Dispatching.Table (J).Last_Priority;
            end if;
         end loop;

         --  Now establish tables to be used for consistency checking

         declare
            --  The following record type is used to record locations of the
            --  Priority_Specific_Dispatching pragmas applying to the Priority.

            type Specific_Dispatching_Entry is record
               Dispatching_Policy : Character := ' ';
               --  First character (upper case) of corresponding policy name

               Afile : ALI_Id := No_ALI_Id;
               --  ALI file that generated Priority Specific Dispatching
               --  entry for consistency message.

               Loc : Nat := 0;
               --  Line numbers from Priority_Specific_Dispatching pragma
            end record;

            PSD_Table  : array (0 .. Max_Prio) of Specific_Dispatching_Entry :=
              (others => Specific_Dispatching_Entry'
                 (Dispatching_Policy => ' ',
                  Afile              => No_ALI_Id,
                  Loc                => 0));
            --  Array containing an entry per priority containing the location
            --  where there is a Priority_Specific_Dispatching pragma that
            --  applies to the priority.

         begin
            for F in ALIs.First .. ALIs.Last loop
               for K in ALIs.Table (F).First_Specific_Dispatching ..
                        ALIs.Table (F).Last_Specific_Dispatching
               loop
                  declare
                     DTK : Specific_Dispatching_Record
                             renames Specific_Dispatching.Table (K);
                  begin
                     --  Check whether pragma Task_Dispatching_Policy and
                     --  pragma Priority_Specific_Dispatching are used in the
                     --  same partition.

                     if Task_Dispatching_Policy_Specified /= ' ' then
                        Error_Msg_File_1 := ALIs.Table (F).Sfile;
                        Error_Msg_File_2 :=
                          ALIs.Table (TDP_Pragma_Afile).Sfile;

                        Error_Msg_Nat_1 := DTK.PSD_Pragma_Line;

                        Consistency_Error_Msg
                          ("Priority_Specific_Dispatching at {:#" &
                           " incompatible with Task_Dispatching_Policy at {");
                     end if;

                     --  Ceiling_Locking must also be specified for a partition
                     --  with at least one Priority_Specific_Dispatching
                     --  pragma.

                     if Locking_Policy_Specified /= ' '
                       and then Locking_Policy_Specified /= 'C'
                     then
                        for A in ALIs.First .. ALIs.Last loop
                           if ALIs.Table (A).Locking_Policy /= ' '
                             and then ALIs.Table (A).Locking_Policy /= 'C'
                           then
                              Error_Msg_File_1 := ALIs.Table (F).Sfile;
                              Error_Msg_File_2 := ALIs.Table (A).Sfile;

                              Error_Msg_Nat_1  := DTK.PSD_Pragma_Line;

                              Consistency_Error_Msg
                                ("Priority_Specific_Dispatching at {:#" &
                                 " incompatible with Locking_Policy at {");
                           end if;
                        end loop;
                     end if;

                     --  Check overlapping priority ranges

                     Find_Overlapping : for Prio in
                       DTK.First_Priority .. DTK.Last_Priority
                     loop
                        if PSD_Table (Prio).Afile = No_ALI_Id then
                           PSD_Table (Prio) :=
                             (Dispatching_Policy => DTK.Dispatching_Policy,
                              Afile => F, Loc => DTK.PSD_Pragma_Line);

                        elsif PSD_Table (Prio).Dispatching_Policy /=
                              DTK.Dispatching_Policy

                        then
                           Error_Msg_File_1 :=
                             ALIs.Table (PSD_Table (Prio).Afile).Sfile;
                           Error_Msg_File_2 := ALIs.Table (F).Sfile;
                           Error_Msg_Nat_1  := PSD_Table (Prio).Loc;
                           Error_Msg_Nat_2  := DTK.PSD_Pragma_Line;

                           Consistency_Error_Msg
                             ("overlapping priority ranges at {:# and {:#");

                           exit Find_Overlapping;
                        end if;
                     end loop Find_Overlapping;
                  end;
               end loop;
            end loop;
         end;
      end if;
   end Check_Consistent_Dispatching_Policy;

   ---------------------------------------------------
   -- Check_Consistent_Dynamic_Elaboration_Checking --
   ---------------------------------------------------

   --  The rule here is that if a unit has dynamic elaboration checks,
   --  then any unit it withs must meet one of the following criteria:

   --    1. There is a pragma Elaborate_All for the with'ed unit
   --    2. The with'ed unit was compiled with dynamic elaboration checks
   --    3. The with'ed unit has pragma Preelaborate or Pure
   --    4. It is an internal GNAT unit (including children of GNAT)
   --    5. It is an interface of a Stand-Alone Library

   procedure Check_Consistent_Dynamic_Elaboration_Checking is
   begin
      if Dynamic_Elaboration_Checks_Specified then
         for U in First_Unit_Entry .. Units.Last loop
            declare
               UR : Unit_Record renames Units.Table (U);

            begin
               if UR.Dynamic_Elab then
                  for W in UR.First_With .. UR.Last_With loop
                     declare
                        WR : With_Record renames Withs.Table (W);

                     begin
                        if Get_Name_Table_Int (WR.Uname) /= 0 then
                           declare
                              WU : Unit_Record renames
                                     Units.Table
                                       (Unit_Id
                                         (Get_Name_Table_Int (WR.Uname)));

                           begin
                              --  Case 1. Elaborate_All for with'ed unit

                              if WR.Elaborate_All then
                                 null;

                              --  Case 2. With'ed unit has dynamic elab checks

                              elsif WU.Dynamic_Elab then
                                 null;

                              --  Case 3. With'ed unit is Preelaborate or Pure

                              elsif WU.Preelab or else WU.Pure then
                                 null;

                              --  Case 4. With'ed unit is internal file

                              elsif Is_Internal_File_Name (WU.Sfile) then
                                 null;

                              --  Case 5. With'ed unit is a SAL interface

                              elsif WU.SAL_Interface then
                                 null;

                              --  Issue warning, not one of the safe cases

                              else
                                 Error_Msg_File_1 := UR.Sfile;
                                 Error_Msg
                                   ("?{ has dynamic elaboration checks " &
                                                                 "and with's");

                                 Error_Msg_File_1 := WU.Sfile;
                                 Error_Msg
                                   ("?  { which has static elaboration " &
                                                                     "checks");

                                 Warnings_Detected := Warnings_Detected + 1;
                              end if;
                           end;
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;
      end if;
   end Check_Consistent_Dynamic_Elaboration_Checking;

   ---------------------------------------
   -- Check_Consistent_Interrupt_States --
   ---------------------------------------

   --  The rule is that if the state of a given interrupt is specified
   --  in more than one unit, it must be specified with a consistent state.

   procedure Check_Consistent_Interrupt_States is
      Max_Intrup : Nat;

   begin
      --  If no Interrupt_State entries, nothing to do

      if Interrupt_States.Last < Interrupt_States.First then
         return;
      end if;

      --  First find out the maximum interrupt value

      Max_Intrup := 0;
      for J in Interrupt_States.First .. Interrupt_States.Last loop
         if Interrupt_States.Table (J).Interrupt_Id > Max_Intrup then
            Max_Intrup := Interrupt_States.Table (J).Interrupt_Id;
         end if;
      end loop;

      --  Now establish tables to be used for consistency checking

      declare
         Istate : array (0 .. Max_Intrup) of Character := (others => 'n');
         --  Interrupt state entries, 'u'/'s'/'r' or 'n' to indicate an
         --  entry that has not been set.

         Afile : array (0 .. Max_Intrup) of ALI_Id;
         --  ALI file that generated Istate entry for consistency message

         Loc : array (0 .. Max_Intrup) of Nat;
         --  Line numbers from IS pragma generating Istate entry

         Inum : Nat;
         --  Interrupt number from entry being tested

         Stat : Character;
         --  Interrupt state from entry being tested

         Lnum : Nat;
         --  Line number from entry being tested

      begin
         for F in ALIs.First .. ALIs.Last loop
            for K in ALIs.Table (F).First_Interrupt_State ..
                     ALIs.Table (F).Last_Interrupt_State
            loop
               Inum := Interrupt_States.Table (K).Interrupt_Id;
               Stat := Interrupt_States.Table (K).Interrupt_State;
               Lnum := Interrupt_States.Table (K).IS_Pragma_Line;

               if Istate (Inum) = 'n' then
                  Istate (Inum) := Stat;
                  Afile  (Inum) := F;
                  Loc    (Inum) := Lnum;

               elsif Istate (Inum) /= Stat then
                  Error_Msg_File_1 := ALIs.Table (Afile (Inum)).Sfile;
                  Error_Msg_File_2 := ALIs.Table (F).Sfile;
                  Error_Msg_Nat_1  := Loc (Inum);
                  Error_Msg_Nat_2  := Lnum;

                  Consistency_Error_Msg
                    ("inconsistent interrupt states at {:# and {:#");
               end if;
            end loop;
         end loop;
      end;
   end Check_Consistent_Interrupt_States;

   -------------------------------------
   -- Check_Consistent_Locking_Policy --
   -------------------------------------

   --  The rule is that all files for which the locking policy is
   --  significant must be compiled with the same setting.

   procedure Check_Consistent_Locking_Policy is
   begin
      --  First search for a unit specifying a policy and then
      --  check all remaining units against it.

      Find_Policy : for A1 in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A1).Locking_Policy /= ' ' then
            Check_Policy : declare
               Policy : constant Character := ALIs.Table (A1).Locking_Policy;

            begin
               for A2 in A1 + 1 .. ALIs.Last loop
                  if ALIs.Table (A2).Locking_Policy /= ' '
                       and then
                     ALIs.Table (A2).Locking_Policy /= Policy
                  then
                     Error_Msg_File_1 := ALIs.Table (A1).Sfile;
                     Error_Msg_File_2 := ALIs.Table (A2).Sfile;

                     Consistency_Error_Msg
                       ("{ and { compiled with different locking policies");
                     exit Find_Policy;
                  end if;
               end loop;
            end Check_Policy;

            exit Find_Policy;
         end if;
      end loop Find_Policy;
   end Check_Consistent_Locking_Policy;

   ----------------------------------------------
   -- Check_Consistent_No_Component_Reordering --
   ----------------------------------------------

   --  This routine checks for a consistent No_Component_Reordering setting.
   --  Note that internal units are excluded from this check, since we don't
   --  in any case allow the pragma to affect types in internal units, and
   --  there is thus no requirement to recompile the run-time with the setting.

   procedure Check_Consistent_No_Component_Reordering is
      OK : Boolean := True;
   begin
      --  Check that all entries have No_Component_Reordering set

      for A1 in ALIs.First .. ALIs.Last loop
         if not Is_Internal_File_Name (ALIs.Table (A1).Sfile)
           and then not ALIs.Table (A1).No_Component_Reordering
         then
            OK := False;
            exit;
         end if;
      end loop;

      --  All do, return

      if OK then
         return;
      end if;

      --  Here we have an inconsistency

      Consistency_Error_Msg
        ("some but not all files compiled with No_Component_Reordering");

      Write_Eol;
      Write_Str ("files compiled with No_Component_Reordering");
      Write_Eol;

      for A1 in ALIs.First .. ALIs.Last loop
         if not Is_Internal_File_Name (ALIs.Table (A1).Sfile)
           and then ALIs.Table (A1).No_Component_Reordering
         then
            Write_Str ("  ");
            Write_Name (ALIs.Table (A1).Sfile);
            Write_Eol;
         end if;
      end loop;

      Write_Eol;
      Write_Str ("files compiled without No_Component_Reordering");
      Write_Eol;

      for A1 in ALIs.First .. ALIs.Last loop
         if not Is_Internal_File_Name (ALIs.Table (A1).Sfile)
           and then not ALIs.Table (A1).No_Component_Reordering
         then
            Write_Str ("  ");
            Write_Name (ALIs.Table (A1).Sfile);
            Write_Eol;
         end if;
      end loop;
   end Check_Consistent_No_Component_Reordering;

   ----------------------------------------
   -- Check_Consistent_Normalize_Scalars --
   ----------------------------------------

   --  The rule is that if any unit is compiled with Normalize_Scalars,
   --  then all other units in the partition must also be compiled with
   --  Normalize_Scalars in effect.

   --  There is some issue as to whether this consistency check is desirable,
   --  it is certainly required at the moment by the RM. We should keep a watch
   --  on the ARG and HRG deliberations here. GNAT no longer depends on this
   --  consistency (it used to do so, but that is no longer the case, since
   --  pragma Initialize_Scalars pragma does not require consistency.)

   procedure Check_Consistent_Normalize_Scalars is
   begin
      if Normalize_Scalars_Specified and No_Normalize_Scalars_Specified then
         Consistency_Error_Msg
              ("some but not all files compiled with Normalize_Scalars");

         Write_Eol;
         Write_Str ("files compiled with Normalize_Scalars");
         Write_Eol;

         for A1 in ALIs.First .. ALIs.Last loop
            if ALIs.Table (A1).Normalize_Scalars then
               Write_Str ("  ");
               Write_Name (ALIs.Table (A1).Sfile);
               Write_Eol;
            end if;
         end loop;

         Write_Eol;
         Write_Str ("files compiled without Normalize_Scalars");
         Write_Eol;

         for A1 in ALIs.First .. ALIs.Last loop
            if not ALIs.Table (A1).Normalize_Scalars then
               Write_Str ("  ");
               Write_Name (ALIs.Table (A1).Sfile);
               Write_Eol;
            end if;
         end loop;
      end if;
   end Check_Consistent_Normalize_Scalars;

   -----------------------------------------
   -- Check_Consistent_Optimize_Alignment --
   -----------------------------------------

   --  The rule is that all units which depend on the global default setting
   --  of Optimize_Alignment must be compiled with the same setting for this
   --  default. Units which specify an explicit local value for this setting
   --  are exempt from the consistency rule (this includes all internal units).

   procedure Check_Consistent_Optimize_Alignment is
      OA_Setting : Character := ' ';
      --  Reset when we find a unit that depends on the default and does
      --  not have a local specification of the Optimize_Alignment setting.

      OA_Unit : Unit_Id := No_Unit_Id;
      --  Id of unit from which OA_Setting was set

      C : Character;

   begin
      for U in First_Unit_Entry .. Units.Last loop
         C := Units.Table (U).Optimize_Alignment;

         if C /= 'L' then
            if OA_Setting = ' ' then
               OA_Setting := C;
               OA_Unit := U;

            elsif OA_Setting = C then
               null;

            else
               pragma Assert (Present (OA_Unit));
               Error_Msg_Unit_1 := Units.Table (OA_Unit).Uname;
               Error_Msg_Unit_2 := Units.Table (U).Uname;

               Consistency_Error_Msg
                 ("$ and $ compiled with different "
                  & "default Optimize_Alignment settings");
               return;
            end if;
         end if;
      end loop;
   end Check_Consistent_Optimize_Alignment;

   ---------------------------------------------------
   -- Check_Consistent_Partition_Elaboration_Policy --
   ---------------------------------------------------

   --  The rule is that all files for which the partition elaboration policy is
   --  significant must be compiled with the same setting.

   procedure Check_Consistent_Partition_Elaboration_Policy is
   begin
      --  First search for a unit specifying a policy and then
      --  check all remaining units against it.

      Find_Policy : for A1 in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A1).Partition_Elaboration_Policy /= ' ' then
            Check_Policy : declare
               Policy : constant Character :=
                  ALIs.Table (A1).Partition_Elaboration_Policy;

            begin
               for A2 in A1 + 1 .. ALIs.Last loop
                  if ALIs.Table (A2).Partition_Elaboration_Policy /= ' '
                       and then
                     ALIs.Table (A2).Partition_Elaboration_Policy /= Policy
                  then
                     Error_Msg_File_1 := ALIs.Table (A1).Sfile;
                     Error_Msg_File_2 := ALIs.Table (A2).Sfile;

                     Consistency_Error_Msg
                       ("{ and { compiled with different partition "
                          & "elaboration policies");
                     exit Find_Policy;
                  end if;
               end loop;
            end Check_Policy;

            --  A No_Task_Hierarchy restriction must be specified for the
            --  Sequential policy (RM H.6(6/2)).

            if Partition_Elaboration_Policy_Specified = 'S'
              and then not Cumulative_Restrictions.Set (No_Task_Hierarchy)
            then
               Error_Msg_File_1 := ALIs.Table (A1).Sfile;
               Error_Msg
                 ("{ has sequential partition elaboration policy, but no");
               Error_Msg
                 ("pragma Restrictions (No_Task_Hierarchy) was specified");
            end if;

            exit Find_Policy;
         end if;
      end loop Find_Policy;
   end Check_Consistent_Partition_Elaboration_Policy;

   -------------------------------------
   -- Check_Consistent_Queuing_Policy --
   -------------------------------------

   --  The rule is that all files for which the queuing policy is
   --  significant must be compiled with the same setting.

   procedure Check_Consistent_Queuing_Policy is
   begin
      --  First search for a unit specifying a policy and then
      --  check all remaining units against it.

      Find_Policy : for A1 in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A1).Queuing_Policy /= ' ' then
            Check_Policy : declare
               Policy : constant Character := ALIs.Table (A1).Queuing_Policy;
            begin
               for A2 in A1 + 1 .. ALIs.Last loop
                  if ALIs.Table (A2).Queuing_Policy /= ' '
                       and then
                     ALIs.Table (A2).Queuing_Policy /= Policy
                  then
                     Error_Msg_File_1 := ALIs.Table (A1).Sfile;
                     Error_Msg_File_2 := ALIs.Table (A2).Sfile;

                     Consistency_Error_Msg
                       ("{ and { compiled with different queuing policies");
                     exit Find_Policy;
                  end if;
               end loop;
            end Check_Policy;

            exit Find_Policy;
         end if;
      end loop Find_Policy;
   end Check_Consistent_Queuing_Policy;

   -----------------------------------
   -- Check_Consistent_Restrictions --
   -----------------------------------

   --  The rule is that if a restriction is specified in any unit, then all
   --  units must obey the restriction. The check applies only to restrictions
   --  which require partition wide consistency, and not to internal units.

   procedure Check_Consistent_Restrictions is
      Restriction_File_Output : Boolean;
      --  Shows if we have output header messages for restriction violation

      procedure Print_Restriction_File (R : All_Restrictions);
      --  Print header line for R if not printed yet

      ----------------------------
      -- Print_Restriction_File --
      ----------------------------

      procedure Print_Restriction_File (R : All_Restrictions) is
      begin
         if not Restriction_File_Output then
            Restriction_File_Output := True;

            --  Find an ali file specifying the restriction

            for A in ALIs.First .. ALIs.Last loop
               if ALIs.Table (A).Restrictions.Set (R)
                 and then (R in All_Boolean_Restrictions
                             or else ALIs.Table (A).Restrictions.Value (R) =
                                     Cumulative_Restrictions.Value (R))
               then
                  --  We have found that ALI file A specifies the restriction
                  --  that is being violated (the minimum value is specified
                  --  in the case of a parameter restriction).

                  declare
                     M1 : constant String := "{ has restriction ";
                     S  : constant String := Restriction_Id'Image (R);
                     M2 : String (1 .. 2000); -- big enough
                     P  : Integer;

                  begin
                     Name_Buffer (1 .. S'Length) := S;
                     Name_Len := S'Length;
                     Set_Casing (Mixed_Case);

                     M2 (M1'Range) := M1;
                     P := M1'Length + 1;
                     M2 (P .. P + S'Length - 1) := Name_Buffer (1 .. S'Length);
                     P := P + S'Length;

                     if R in All_Parameter_Restrictions then
                        M2 (P .. P + 4) := " => #";
                        Error_Msg_Nat_1 :=
                          Int (Cumulative_Restrictions.Value (R));
                        P := P + 5;
                     end if;

                     Error_Msg_File_1 := ALIs.Table (A).Sfile;
                     Consistency_Error_Msg (M2 (1 .. P - 1));
                     Consistency_Error_Msg
                       ("but the following files violate this restriction:");
                     return;
                  end;
               end if;
            end loop;
         end if;
      end Print_Restriction_File;

   --  Start of processing for Check_Consistent_Restrictions

   begin
      --  We used to have a special test here:

         --  A special test, if we have a main program, then if it has an
         --  allocator in the body, this is considered to be a violation of
         --  the restriction No_Allocators_After_Elaboration. We just mark
         --  this restriction and then the normal circuit will flag it.

      --  But we don't do that any more, because in the final version of Ada
      --  2012, it is statically illegal to have an allocator in a library-
      --  level subprogram, so we don't need this bind time test any more.
      --  If we have a main program with parameters (which GNAT allows), then
      --  allocators in that will be caught by the run-time check.

      --  Loop through all restriction violations

      for R in All_Restrictions loop

         --  Check for violation of this restriction

         if Cumulative_Restrictions.Set (R)
           and then Cumulative_Restrictions.Violated (R)
           and then (R in Partition_Boolean_Restrictions
                       or else (R in All_Parameter_Restrictions
                                   and then
                                     Cumulative_Restrictions.Count (R) >
                                     Cumulative_Restrictions.Value (R)))
         then
            Restriction_File_Output := False;

            --  Loop through files looking for violators

            for A2 in ALIs.First .. ALIs.Last loop
               declare
                  T : ALIs_Record renames ALIs.Table (A2);

               begin
                  if T.Restrictions.Violated (R) then

                     --  We exclude predefined files from the list of
                     --  violators. This should be rethought. It is not
                     --  clear that this is the right thing to do, that
                     --  is particularly the case for restricted runtimes.

                     if not Is_Internal_File_Name (T.Sfile) then

                        --  Case of Boolean restriction, just print file name

                        if R in All_Boolean_Restrictions then
                           Print_Restriction_File (R);
                           Error_Msg_File_1 := T.Sfile;
                           Consistency_Error_Msg ("  {");

                        --  Case of Parameter restriction where violation
                        --  count exceeds restriction value, print file
                        --  name and count, adding "at least" if the
                        --  exact count is not known.

                        elsif R in Checked_Add_Parameter_Restrictions
                          or else T.Restrictions.Count (R) >
                          Cumulative_Restrictions.Value (R)
                        then
                           Print_Restriction_File (R);
                           Error_Msg_File_1 := T.Sfile;
                           Error_Msg_Nat_1 := Int (T.Restrictions.Count (R));

                           if T.Restrictions.Unknown (R) then
                              Consistency_Error_Msg
                                ("  { (count = at least #)");
                           else
                              Consistency_Error_Msg
                                ("  { (count = #)");
                           end if;
                        end if;
                     end if;
                  end if;
               end;
            end loop;
         end if;
      end loop;

      --  Now deal with No_Dependence indications. Note that we put the loop
      --  through entries in the no dependency table first, since this loop
      --  is most often empty (no such pragma Restrictions in use).

      for ND in No_Deps.First .. No_Deps.Last loop
         declare
            ND_Unit : constant Name_Id := No_Deps.Table (ND).No_Dep_Unit;
         begin
            for J in ALIs.First .. ALIs.Last loop
               declare
                  A : ALIs_Record renames ALIs.Table (J);
               begin
                  for K in A.First_Unit .. A.Last_Unit loop
                     declare
                        U : Unit_Record renames Units.Table (K);
                     begin
                        --  Exclude runtime units from this check since the
                        --  user does not care how a runtime unit is
                        --  implemented.

                        if not Is_Internal_File_Name (U.Sfile) then
                           for L in U.First_With .. U.Last_With loop
                              if Same_Unit (Withs.Table (L).Uname, ND_Unit)
                              then
                                 Error_Msg_File_1 := U.Sfile;
                                 Error_Msg_Name_1 := ND_Unit;
                                 Consistency_Error_Msg
                                   ("file { violates restriction " &
                                    "No_Dependence => %");
                              end if;
                           end loop;
                        end if;
                     end;
                  end loop;
               end;
            end loop;
         end;
      end loop;
   end Check_Consistent_Restrictions;

   ------------------------------------------------------------
   -- Check_Consistent_Restriction_No_Default_Initialization --
   ------------------------------------------------------------

   --  The Restriction (No_Default_Initialization) has special consistency
   --  rules. The rule is that no unit compiled without this restriction
   --  that violates the restriction can WITH a unit that is compiled with
   --  the restriction.

   procedure Check_Consistent_Restriction_No_Default_Initialization is
   begin
      --  Nothing to do if no one set this restriction

      if not Cumulative_Restrictions.Set (No_Default_Initialization) then
         return;
      end if;

      --  Nothing to do if no one violates the restriction

      if not Cumulative_Restrictions.Violated (No_Default_Initialization) then
         return;
      end if;

      --  Otherwise we go into a full scan to find possible problems

      for U in Units.First .. Units.Last loop
         declare
            UTE : Unit_Record renames Units.Table (U);
            ATE : ALIs_Record renames ALIs.Table (UTE.My_ALI);

         begin
            if ATE.Restrictions.Violated (No_Default_Initialization) then
               for W in UTE.First_With .. UTE.Last_With loop
                  declare
                     AFN : constant File_Name_Type := Withs.Table (W).Afile;

                  begin
                     --  The file name may not be present for withs of certain
                     --  generic run-time files. The test can be safely left
                     --  out in such cases anyway.

                     if AFN /= No_File then
                        declare
                           WAI : constant ALI_Id :=
                             ALI_Id (Get_Name_Table_Int (AFN));
                           WTE : ALIs_Record renames ALIs.Table (WAI);

                        begin
                           if WTE.Restrictions.Set
                               (No_Default_Initialization)
                           then
                              Error_Msg_Unit_1 := UTE.Uname;
                              Consistency_Error_Msg
                                ("unit $ compiled without restriction "
                                 & "No_Default_Initialization");
                              Error_Msg_Unit_1 := Withs.Table (W).Uname;
                              Consistency_Error_Msg
                                ("withs unit $, compiled with restriction "
                                 & "No_Default_Initialization");
                           end if;
                        end;
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Check_Consistent_Restriction_No_Default_Initialization;

   ----------------------------------
   -- Check_Consistent_SSO_Default --
   ----------------------------------

   --  This routine checks for a consistent SSO default setting. Note that
   --  internal units are excluded from this check, since we don't in any
   --  case allow the pragma to affect types in internal units, and there
   --  is thus no requirement to recompile the run-time with the default set.

   procedure Check_Consistent_SSO_Default is
      Default : Character;

   begin
      Default := ALIs.Table (ALIs.First).SSO_Default;

      --  The default must be set from a non-internal unit

      pragma Assert
        (not Is_Internal_File_Name (ALIs.Table (ALIs.First).Sfile));

      --  Check all entries match the default above from the first entry

      for A1 in ALIs.First + 1 .. ALIs.Last loop
         if not Is_Internal_File_Name (ALIs.Table (A1).Sfile)
           and then ALIs.Table (A1).SSO_Default /= Default
         then
            Default := '?';
            exit;
         end if;
      end loop;

      --  All match, return

      if Default /= '?' then
         return;
      end if;

      --  Here we have a mismatch

      Consistency_Error_Msg
        ("files not compiled with same Default_Scalar_Storage_Order");

      Write_Eol;
      Write_Str ("files compiled with High_Order_First");
      Write_Eol;

      for A1 in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A1).SSO_Default = 'H' then
            Write_Str ("  ");
            Write_Name (ALIs.Table (A1).Sfile);
            Write_Eol;
         end if;
      end loop;

      Write_Eol;
      Write_Str ("files compiled with Low_Order_First");
      Write_Eol;

      for A1 in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A1).SSO_Default = 'L' then
            Write_Str ("  ");
            Write_Name (ALIs.Table (A1).Sfile);
            Write_Eol;
         end if;
      end loop;

      Write_Eol;
      Write_Str ("files compiled with no Default_Scalar_Storage_Order");
      Write_Eol;

      for A1 in ALIs.First .. ALIs.Last loop
         if not Is_Internal_File_Name (ALIs.Table (A1).Sfile)
           and then ALIs.Table (A1).SSO_Default = ' '
         then
            Write_Str ("  ");
            Write_Name (ALIs.Table (A1).Sfile);
            Write_Eol;
         end if;
      end loop;
   end Check_Consistent_SSO_Default;

   -----------------------------------------
   -- Check_Consistent_Exception_Handling --
   -----------------------------------------

   --  All units must have the same exception handling mechanism.

   procedure Check_Consistent_Exception_Handling is
   begin
      Check_Mechanism : for A1 in ALIs.First + 1 .. ALIs.Last loop
         if (ALIs.Table (A1).Zero_Cost_Exceptions /=
              ALIs.Table (ALIs.First).Zero_Cost_Exceptions)
           or else
            (ALIs.Table (A1).Frontend_Exceptions /=
              ALIs.Table (ALIs.First).Frontend_Exceptions)
         then
            Error_Msg_File_1 := ALIs.Table (A1).Sfile;
            Error_Msg_File_2 := ALIs.Table (ALIs.First).Sfile;

            Consistency_Error_Msg
              ("{ and { compiled with different exception handling "
               & "mechanisms");
         end if;
      end loop Check_Mechanism;
   end Check_Consistent_Exception_Handling;

   -------------------------------
   -- Check_Duplicated_Subunits --
   -------------------------------

   procedure Check_Duplicated_Subunits is
   begin
      for J in Sdep.First .. Sdep.Last loop
         if Sdep.Table (J).Subunit_Name /= No_Name then
            Get_Decoded_Name_String (Sdep.Table (J).Subunit_Name);
            Name_Len := Name_Len + 2;
            Name_Buffer (Name_Len - 1) := '%';

            --  See if there is a body or spec with the same name

            for K in Boolean loop
               if K then
                  Name_Buffer (Name_Len) := 'b';
               else
                  Name_Buffer (Name_Len) := 's';
               end if;

               declare
                  Unit : constant Unit_Name_Type := Name_Find;
                  Info : constant Int := Get_Name_Table_Int (Unit);

               begin
                  if Info /= 0 then
                     Set_Standard_Error;
                     Write_Str ("error: subunit """);
                     Write_Name_Decoded (Sdep.Table (J).Subunit_Name);
                     Write_Str (""" in file """);
                     Write_Name_Decoded (Sdep.Table (J).Sfile);
                     Write_Char ('"');
                     Write_Eol;
                     Write_Str ("       has same name as unit """);
                     Write_Unit_Name (Units.Table (Unit_Id (Info)).Uname);
                     Write_Str (""" found in file """);
                     Write_Name_Decoded (Units.Table (Unit_Id (Info)).Sfile);
                     Write_Char ('"');
                     Write_Eol;
                     Write_Str ("       this is not allowed within a single "
                                & "partition (RM 10.2(19))");
                     Write_Eol;
                     Osint.Exit_Program (Osint.E_Fatal);
                  end if;
               end;
            end loop;
         end if;
      end loop;
   end Check_Duplicated_Subunits;

   --------------------
   -- Check_Versions --
   --------------------

   procedure Check_Versions is
      VL : constant Natural := ALIs.Table (ALIs.First).Ver_Len;

   begin
      for A in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A).Ver_Len /= VL
           or else ALIs.Table (A).Ver          (1 .. VL) /=
                   ALIs.Table (ALIs.First).Ver (1 .. VL)
         then
            --  Version mismatch found; generate error message.

            declare
               use Gnatvsn;

               Prefix : constant String :=
                 Verbose_Library_Version
                   (1 .. Verbose_Library_Version'Length
                           - Library_Version'Length);

               type ALI_Version is record
                  Primary, Secondary : Int range -1 .. Int'Last;
               end record;

               No_Version : constant ALI_Version := (-1, -1);

               function Remove_Prefix (S : String) return String is
                 (S (S'First + Prefix'Length .. S'Last));

               function Extract_Version (S : String) return ALI_Version;
               --  Attempts to extract and return a pair of nonnegative library
               --  version numbers from the given string; if unsuccessful,
               --  then returns No_Version.

               ---------------------
               -- Extract_Version --
               ---------------------

               function Extract_Version (S : String) return ALI_Version is
                  pragma Assert (S'First = 1);

                  function Int_Value (Img : String) return Int;
                  --  Using Int'Value leads to complications in
                  --  building the binder, so DIY.

                  ---------------
                  -- Int_Value --
                  ---------------

                  function Int_Value (Img : String) return Int is
                     Result : Nat := 0;
                  begin
                     if Img'Length in 1 .. 9
                       and then (for all C of Img => C in '0' .. '9')
                     then
                        for C of Img loop
                           Result := (10 * Result) +
                             (Character'Pos (C) - Character'Pos ('0'));
                        end loop;
                        return Result;
                     else
                        return -1;
                     end if;
                  end Int_Value;

               begin
                  if S'Length > Prefix'Length
                    and then S (1 .. Prefix'Length) = Prefix
                  then
                     declare
                        Suffix    : constant String := Remove_Prefix (S);
                        Dot_Found : Boolean := False;
                        Primary, Secondary : Int;
                     begin
                        for Dot_Index in Suffix'Range loop
                           if Suffix (Dot_Index) = '.' then
                              Dot_Found := True;
                              Primary :=
                                Int_Value (Suffix (Suffix'First
                                                   .. Dot_Index - 1));
                              Secondary :=
                                Int_Value (Suffix (Dot_Index + 1
                                                   .. Suffix'Last));
                              exit;
                           end if;
                        end loop;

                        if not Dot_Found then
                           Primary   := Int_Value (Suffix);
                           Secondary := 0;
                        end if;

                        if (Primary /= -1) and (Secondary /= -1) then
                           return (Primary   => Primary,
                                   Secondary => Secondary);
                        end if;
                     end;
                  end if;
                  return No_Version;
               end Extract_Version;

               --  Local constants

               V1_Text : constant String :=
                 ALIs.Table (A).Ver (1 .. ALIs.Table (A).Ver_Len);
               V2_Text : constant String :=
                 ALIs.Table (ALIs.First).Ver (1 .. VL);
               V1      : constant ALI_Version := Extract_Version (V1_Text);
               V2      : constant ALI_Version := Extract_Version (V2_Text);

               Include_Version_Numbers_In_Message : constant Boolean :=
                 (V1 /= V2) and (V1 /= No_Version) and (V2 /= No_Version);
            begin
               Error_Msg_File_1 := ALIs.Table (A).Sfile;
               Error_Msg_File_2 := ALIs.Table (ALIs.First).Sfile;

               if Include_Version_Numbers_In_Message then
                  if V1.Secondary = V2.Secondary then
                     --  Excluding equal secondary values from error
                     --  message text matters for generating reproducible
                     --  regression test outputs.

                     Error_Msg_Nat_1 := V1.Primary;
                     Error_Msg_Nat_2 := V2.Primary;
                     Consistency_Error_Msg
                       ("{ and { compiled with different GNAT versions"
                        & ", v# and v#");
                  else
                     Consistency_Error_Msg
                       ("{ and { compiled with different GNAT versions"
                        & ", v"
                        & Remove_Prefix (V1_Text)
                        & " and v"
                        & Remove_Prefix (V2_Text));
                  end if;
               else
                  Consistency_Error_Msg
                    ("{ and { compiled with different GNAT versions");
               end if;
            end;
         end if;
      end loop;
   end Check_Versions;

   ---------------------------
   -- Consistency_Error_Msg --
   ---------------------------

   procedure Consistency_Error_Msg (Msg : String) is
   begin
      if Tolerate_Consistency_Errors then

         --  If consistency errors are tolerated,
         --  output the message as a warning.

         Error_Msg ('?' & Msg);

      --  Otherwise the consistency error is a true error

      else
         Error_Msg (Msg);
      end if;
   end Consistency_Error_Msg;

   ---------------
   -- Same_Unit --
   ---------------

   function Same_Unit (U1 : Unit_Name_Type; U2 : Name_Id) return Boolean is
   begin
      --  Note, the string U1 has a terminating %s or %b, U2 does not

      if Length_Of_Name (U1) - 2 = Length_Of_Name (U2) then
         Get_Name_String (U1);

         declare
            U1_Str : constant String := Name_Buffer (1 .. Name_Len - 2);
         begin
            Get_Name_String (U2);
            return U1_Str = Name_Buffer (1 .. Name_Len);
         end;

      else
         return False;
      end if;
   end Same_Unit;

end Bcheck;
