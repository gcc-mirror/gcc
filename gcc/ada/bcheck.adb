------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               B C H E C K                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
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

with ALI;      use ALI;
with ALI.Util; use ALI.Util;
with Binderr;  use Binderr;
with Butil;    use Butil;
with Casing;   use Casing;
with Fname;    use Fname;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;
with Output;   use Output;
with Rident;   use Rident;
with Types;    use Types;

package body Bcheck is

   --  Local subprograms

   --  The following checking subprograms make up the parts
   --  of the configuration consistency check.

   procedure Check_Consistent_Dynamic_Elaboration_Checking;
   procedure Check_Consistent_Floating_Point_Format;
   procedure Check_Consistent_Locking_Policy;
   procedure Check_Consistent_Normalize_Scalars;
   procedure Check_Consistent_Queuing_Policy;
   procedure Check_Consistent_Zero_Cost_Exception_Handling;
   procedure Check_Partition_Restrictions;

   procedure Consistency_Error_Msg (Msg : String);
   --  Produce an error or a warning message, depending on whether
   --  an inconsistent configuration is permitted or not.

   ------------------------------------
   -- Check_Consistent_Configuration --
   ------------------------------------

   procedure Check_Configuration_Consistency is
   begin
      if Float_Format_Specified /= ' ' then
         Check_Consistent_Floating_Point_Format;
      end if;

      if Queuing_Policy_Specified /= ' ' then
         Check_Consistent_Queuing_Policy;
      end if;

      if Locking_Policy_Specified /= ' ' then
         Check_Consistent_Locking_Policy;
      end if;

      if Zero_Cost_Exceptions_Specified then
         Check_Consistent_Zero_Cost_Exception_Handling;
      end if;

      Check_Consistent_Normalize_Scalars;
      Check_Consistent_Dynamic_Elaboration_Checking;

      Check_Partition_Restrictions;
   end Check_Configuration_Consistency;

   ---------------------------------------------------
   -- Check_Consistent_Dynamic_Elaboration_Checking --
   ---------------------------------------------------

   --  The rule here is that if a unit has dynamic elaboration checks,
   --  then any unit it withs must meeting one of the following criteria:

   --    1. There is a pragma Elaborate_All for the with'ed unit
   --    2. The with'ed unit was compiled with dynamic elaboration checks
   --    3. The with'ed unit has pragma Preelaborate or Pure
   --    4. It is an internal GNAT unit (including children of GNAT)

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
                        if Get_Name_Table_Info (WR.Uname) /= 0 then
                           declare
                              WU : Unit_Record renames
                                     Units.Table
                                       (Unit_Id
                                         (Get_Name_Table_Info (WR.Uname)));

                           begin
                              --  Case 1. Elaborate_All for with'ed unit

                              if WR.Elaborate_All then
                                 null;

                              --  Case 2. With'ed unit has dynamic elab checks

                              elsif WU.Dynamic_Elab then
                                 null;

                              --  Case 3. With'ed unit is Preelaborate or Pure

                              elsif WU.Preelab or WU.Pure then
                                 null;

                              --  Case 4. With'ed unit is internal file

                              elsif Is_Internal_File_Name (WU.Sfile) then
                                 null;

                              --  Issue warning, not one of the safe cases

                              else
                                 Error_Msg_Name_1 := UR.Sfile;
                                 Error_Msg
                                   ("?% has dynamic elaboration checks " &
                                                                 "and with's");

                                 Error_Msg_Name_1 := WU.Sfile;
                                 Error_Msg
                                   ("?  % which has static elaboration " &
                                                                     "checks");

                                 Warnings_Detected := Warnings_Detected - 1;
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

   --------------------------------------------
   -- Check_Consistent_Floating_Point_Format --
   --------------------------------------------

   --  The rule is that all files must be compiled with the same setting
   --  for the floating-point format.

   procedure Check_Consistent_Floating_Point_Format is
   begin
      --  First search for a unit specifying a floating-point format and then
      --  check all remaining units against it.

      Find_Format : for A1 in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A1).Float_Format /= ' ' then
            Check_Format : declare
               Format : constant Character := ALIs.Table (A1).Float_Format;
            begin
               for A2 in A1 + 1 .. ALIs.Last loop
                  if ALIs.Table (A2).Float_Format /= Format then
                     Error_Msg_Name_1 := ALIs.Table (A1).Sfile;
                     Error_Msg_Name_2 := ALIs.Table (A2).Sfile;

                     Consistency_Error_Msg
                       ("% and % compiled with different " &
                        "floating-point representations");
                     exit Find_Format;
                  end if;
               end loop;
            end Check_Format;

            exit Find_Format;
         end if;
      end loop Find_Format;
   end Check_Consistent_Floating_Point_Format;

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
                  if ALIs.Table (A2).Locking_Policy /= ' ' and
                     ALIs.Table (A2).Locking_Policy /= Policy
                  then
                     Error_Msg_Name_1 := ALIs.Table (A1).Sfile;
                     Error_Msg_Name_2 := ALIs.Table (A2).Sfile;

                     Consistency_Error_Msg
                       ("% and % compiled with different locking policies");
                     exit Find_Policy;
                  end if;
               end loop;
            end Check_Policy;

            exit Find_Policy;
         end if;
      end loop Find_Policy;
   end Check_Consistent_Locking_Policy;

   ----------------------------------------
   -- Check_Consistent_Normalize_Scalars --
   ----------------------------------------

   --  The rule is that if any unit is compiled with Normalized_Scalars,
   --  then all other units in the partition must also be compiled with
   --  Normalized_Scalars in effect.

   --  There is some issue as to whether this consistency check is
   --  desirable, it is certainly required at the moment by the RM.
   --  We should keep a watch on the ARG and HRG deliberations here.
   --  GNAT no longer depends on this consistency (it used to do so,
   --  but that has been corrected in the latest version, since the
   --  Initialize_Scalars pragma does not require consistency.

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
                     Error_Msg_Name_1 := ALIs.Table (A1).Sfile;
                     Error_Msg_Name_2 := ALIs.Table (A2).Sfile;

                     Consistency_Error_Msg
                       ("% and % compiled with different queuing policies");
                     exit Find_Policy;
                  end if;
               end loop;
            end Check_Policy;

            exit Find_Policy;
         end if;
      end loop Find_Policy;
   end Check_Consistent_Queuing_Policy;

   ---------------------------------------------------
   -- Check_Consistent_Zero_Cost_Exception_Handling --
   ---------------------------------------------------

   --  Check consistent zero cost exception handling. The rule is that
   --  all units must have the same exception handling mechanism.

   procedure Check_Consistent_Zero_Cost_Exception_Handling is
   begin
      Check_Mechanism : for A1 in ALIs.First + 1 .. ALIs.Last loop
         if ALIs.Table (A1).Zero_Cost_Exceptions /=
            ALIs.Table (ALIs.First).Zero_Cost_Exceptions

         then
            Error_Msg_Name_1 := ALIs.Table (A1).Sfile;
            Error_Msg_Name_2 := ALIs.Table (ALIs.First).Sfile;

            Consistency_Error_Msg ("% and % compiled with different "
                                            & "exception handling mechanisms");
         end if;
      end loop Check_Mechanism;
   end Check_Consistent_Zero_Cost_Exception_Handling;

   ----------------------------------
   -- Check_Partition_Restrictions --
   ----------------------------------

   --  The rule is that if a restriction is specified in any unit,
   --  then all units must obey the restriction. The check applies
   --  only to restrictions which require partition wide consistency,
   --  and not to internal units.

   --  The check is done in two steps. First for every restriction
   --  a unit specifying that restriction is found, if any.
   --  Second, all units are verified against the specified restrictions.

   procedure Check_Partition_Restrictions is
      No_Restriction_List : array (All_Restrictions) of Boolean :=
        (No_Implicit_Conditionals => True,
         --  This could modify and pessimize generated code

         No_Implicit_Dynamic_Code => True,
         --  This could modify and pessimize generated code

         No_Implicit_Loops        => True,
         --  This could modify and pessimize generated code

         No_Recursion             => True,
         --  Not checkable at compile time

         No_Reentrancy            => True,
         --  Not checkable at compile time

         others                   => False);
      --  Define those restrictions that should be output if the gnatbind -r
      --  switch is used. Not all restrictions are output for the reasons given
      --  above in the list, and this array is used to test whether the
      --  corresponding pragma should be listed. True means that it should not
      --  be listed.

      R : array (All_Restrictions) of ALI_Id := (others => No_ALI_Id);
      --  Record the first unit specifying each compilation unit restriction

      V : array (All_Restrictions) of ALI_Id := (others => No_ALI_Id);
      --  Record the last unit violating each partition restriction. Note
      --  that entries in this array that do not correspond to partition
      --  restrictions can never be modified.

      Additional_Restrictions_Listed : Boolean := False;
      --  Set True if we have listed header for restrictions

   begin
      --  Loop to find restrictions

      for A in ALIs.First .. ALIs.Last loop
         for J in All_Restrictions loop
            if R (J) = No_ALI_Id and ALIs.Table (A).Restrictions (J) = 'r' then
               R (J) := A;
            end if;
         end loop;
      end loop;

      --  Loop to find violations

      for A in ALIs.First .. ALIs.Last loop
         for J in All_Restrictions loop
            if ALIs.Table (A).Restrictions (J) = 'v'
               and then not Is_Internal_File_Name (ALIs.Table (A).Sfile)
            then
               --  A violation of a restriction was found

               V (J) := A;

               --  If this is a paritition restriction, and the restriction
               --  was specified in some unit in the partition, then this
               --  is a violation of the consistency requirement, so we
               --  generate an appropriate error message.

               if R (J) /= No_ALI_Id
                 and then J in Partition_Restrictions
               then
                  declare
                     M1 : constant String := "% has Restriction (";
                     S  : constant String := Restriction_Id'Image (J);
                     M2 : String (1 .. M1'Length + S'Length + 1);

                  begin
                     Name_Buffer (1 .. S'Length) := S;
                     Name_Len := S'Length;
                     Set_Casing
                       (Units.Table (ALIs.Table (R (J)).First_Unit).Icasing);

                     M2 (M1'Range) := M1;
                     M2 (M1'Length + 1 .. M2'Last - 1) :=
                                                   Name_Buffer (1 .. S'Length);
                     M2 (M2'Last) := ')';

                     Error_Msg_Name_1 := ALIs.Table (R (J)).Sfile;
                     Consistency_Error_Msg (M2);
                     Error_Msg_Name_1 := ALIs.Table (A).Sfile;
                     Consistency_Error_Msg
                       ("but file % violates this restriction");
                  end;
               end if;
            end if;
         end loop;
      end loop;

      --  List applicable restrictions if option set

      if List_Restrictions then

         --  List any restrictions which were not violated and not specified

         for J in All_Restrictions loop
            if V (J) = No_ALI_Id
              and then R (J) = No_ALI_Id
              and then not No_Restriction_List (J)
            then
               if not Additional_Restrictions_Listed then
                  Write_Eol;
                  Write_Line
                    ("The following additional restrictions may be" &
                     " applied to this partition:");
                  Additional_Restrictions_Listed := True;
               end if;

               Write_Str ("pragma Restrictions (");

               declare
                  S : constant String := Restriction_Id'Image (J);

               begin
                  Name_Len := S'Length;
                  Name_Buffer (1 .. Name_Len) := S;
               end;

               Set_Casing (Mixed_Case);
               Write_Str (Name_Buffer (1 .. Name_Len));
               Write_Str (");");
               Write_Eol;
            end if;
         end loop;
      end if;
   end Check_Partition_Restrictions;

   -----------------------
   -- Check_Consistency --
   -----------------------

   procedure Check_Consistency is
      Src : Source_Id;
      --  Source file Id for this Sdep entry

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

            Src := Source_Id (Get_Name_Table_Info (Sdep.Table (D).Sfile));

            --  If the time stamps match, or all checksums match, then we
            --  are OK, otherwise we have a definite error.

            if Sdep.Table (D).Stamp /= Source.Table (Src).Stamp
              and then not Source.Table (Src).All_Checksums_Match
            then
               Error_Msg_Name_1 := ALIs.Table (A).Sfile;
               Error_Msg_Name_2 := Sdep.Table (D).Sfile;

               --  Two styles of message, depending on whether or not
               --  the updated file is the one that must be recompiled

               if Error_Msg_Name_1 = Error_Msg_Name_2 then
                  if Tolerate_Consistency_Errors then
                     Error_Msg
                        ("?% has been modified and should be recompiled");
                  else
                     Error_Msg
                       ("% has been modified and must be recompiled");
                  end if;

               else
                  if Tolerate_Consistency_Errors then
                     Error_Msg
                       ("?% should be recompiled (% has been modified)");

                  else
                     Error_Msg ("% must be recompiled (% has been modified)");
                  end if;
               end if;

               if (not Tolerate_Consistency_Errors) and Verbose_Mode then
                  declare
                     Msg : constant String := "file % has time stamp ";
                     Buf : String (1 .. Msg'Length + Time_Stamp_Length);

                  begin
                     Buf (1 .. Msg'Length) := Msg;
                     Buf (Msg'Length + 1 .. Buf'Length) :=
                       String (Source.Table (Src).Stamp);
                     Error_Msg_Name_1 := ALIs.Table (A).Sfile;
                     Error_Msg (Buf);

                     Buf (Msg'Length + 1 .. Buf'Length) :=
                       String (Sdep.Table (D).Stamp);
                     Error_Msg_Name_1 := Sdep.Table (D).Sfile;
                     Error_Msg (Buf);
                  end;
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
                  Info : constant Int := Get_Name_Table_Info (Name_Find);

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
            Error_Msg_Name_1 := ALIs.Table (A).Sfile;
            Error_Msg_Name_2 := ALIs.Table (ALIs.First).Sfile;

            Consistency_Error_Msg
               ("% and % compiled with different GNAT versions");
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

         declare
            Warning_Msg : String (1 .. Msg'Length + 1);

         begin
            Warning_Msg (1) := '?';
            Warning_Msg (2 .. Warning_Msg'Last) := Msg;

            Error_Msg (Warning_Msg);
         end;

      --  Otherwise the consistency error is a true error

      else
         Error_Msg (Msg);
      end if;
   end Consistency_Error_Msg;

end Bcheck;
