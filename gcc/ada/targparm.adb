------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                             T A R G P A R M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2025, Free Software Foundation, Inc.         --
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

with Csets;         use Csets;
with Opt;
with Osint;         use Osint;
with Output;        use Output;
with System.OS_Lib; use System.OS_Lib;

package body Targparm is
   use ASCII;

   Parameters_Obtained : Boolean := False;
   --  Set True after first call to Get_Target_Parameters. Used to avoid
   --  reading system.ads more than once, since it cannot change.

   --  The following array defines a tag name for each entry

   type Targparm_Tags is
     (ACR,  --   Always_Compatible_Rep
      ASD,  --   Atomic_Sync_Default
      BDC,  --   Backend_Divide_Checks
      BOC,  --   Backend_Overflow_Checks
      CLA,  --   Command_Line_Args
      CRT,  --   Configurable_Run_Times
      D32,  --   Duration_32_Bits
      DEN,  --   Denorm
      EXS,  --   Exit_Status_Supported
      MOV,  --   Machine_Overflows
      MRN,  --   Machine_Rounds
      PAS,  --   Preallocated_Stacks
      SAG,  --   Support_Aggregates
      SAP,  --   Support_Atomic_Primitives
      SCA,  --   Support_Composite_Assign
      SCC,  --   Support_Composite_Compare
      SCD,  --   Stack_Check_Default
      SCL,  --   Stack_Check_Limits
      SCP,  --   Stack_Check_Probes
      SLS,  --   Support_Long_Shifts
      SNZ,  --   Signed_Zeros
      SSL,  --   Suppress_Standard_Library
      UAM,  --   Use_Ada_Main_Program_Name
      ZCX); --   ZCX_By_Default

   Targparm_Flags : array (Targparm_Tags) of Boolean := (others => False);
   --  Flag is set True if corresponding parameter is scanned

   --  The following list of string constants gives the parameter names

   ACR_Str : aliased constant Source_Buffer := "Always_Compatible_Rep";
   ASD_Str : aliased constant Source_Buffer := "Atomic_Sync_Default";
   BDC_Str : aliased constant Source_Buffer := "Backend_Divide_Checks";
   BOC_Str : aliased constant Source_Buffer := "Backend_Overflow_Checks";
   CLA_Str : aliased constant Source_Buffer := "Command_Line_Args";
   CRT_Str : aliased constant Source_Buffer := "Configurable_Run_Time";
   D32_Str : aliased constant Source_Buffer := "Duration_32_Bits";
   DEN_Str : aliased constant Source_Buffer := "Denorm";
   EXS_Str : aliased constant Source_Buffer := "Exit_Status_Supported";
   MOV_Str : aliased constant Source_Buffer := "Machine_Overflows";
   MRN_Str : aliased constant Source_Buffer := "Machine_Rounds";
   PAS_Str : aliased constant Source_Buffer := "Preallocated_Stacks";
   SAG_Str : aliased constant Source_Buffer := "Support_Aggregates";
   SAP_Str : aliased constant Source_Buffer := "Support_Atomic_Primitives";
   SCA_Str : aliased constant Source_Buffer := "Support_Composite_Assign";
   SCC_Str : aliased constant Source_Buffer := "Support_Composite_Compare";
   SCD_Str : aliased constant Source_Buffer := "Stack_Check_Default";
   SCL_Str : aliased constant Source_Buffer := "Stack_Check_Limits";
   SCP_Str : aliased constant Source_Buffer := "Stack_Check_Probes";
   SLS_Str : aliased constant Source_Buffer := "Support_Long_Shifts";
   SNZ_Str : aliased constant Source_Buffer := "Signed_Zeros";
   SSL_Str : aliased constant Source_Buffer := "Suppress_Standard_Library";
   UAM_Str : aliased constant Source_Buffer := "Use_Ada_Main_Program_Name";
   ZCX_Str : aliased constant Source_Buffer := "ZCX_By_Default";

   --  The following defines a set of pointers to the above strings,
   --  indexed by the tag values.

   type Buffer_Ptr is access constant Source_Buffer;
   Targparm_Str : constant array (Targparm_Tags) of Buffer_Ptr :=
     (ACR => ACR_Str'Access,
      ASD => ASD_Str'Access,
      BDC => BDC_Str'Access,
      BOC => BOC_Str'Access,
      CLA => CLA_Str'Access,
      CRT => CRT_Str'Access,
      D32 => D32_Str'Access,
      DEN => DEN_Str'Access,
      EXS => EXS_Str'Access,
      MOV => MOV_Str'Access,
      MRN => MRN_Str'Access,
      PAS => PAS_Str'Access,
      SAG => SAG_Str'Access,
      SAP => SAP_Str'Access,
      SCA => SCA_Str'Access,
      SCC => SCC_Str'Access,
      SCD => SCD_Str'Access,
      SCL => SCL_Str'Access,
      SCP => SCP_Str'Access,
      SLS => SLS_Str'Access,
      SNZ => SNZ_Str'Access,
      SSL => SSL_Str'Access,
      UAM => UAM_Str'Access,
      ZCX => ZCX_Str'Access);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Set_Profile_Restrictions (P : Profile_Name);
   --  Set Restrictions_On_Target for the given profile

   ---------------------------
   -- Get_Target_Parameters --
   ---------------------------

   --  Version that reads in system.ads

   procedure Get_Target_Parameters
     (Make_Id : Make_Id_Type := null;
      Make_SC : Make_SC_Type := null;
      Set_NOD : Set_NOD_Type := null;
      Set_NSA : Set_NSA_Type := null;
      Set_NUA : Set_NUA_Type := null;
      Set_NUP : Set_NUP_Type := null)
   is
      FD   : File_Descriptor;
      Hi   : Source_Ptr;
      Text : Source_Buffer_Ptr;

   begin
      if Parameters_Obtained then
         return;
      end if;

      Read_Source_File (Name_Find ("system.ads"), 0, Hi, Text, FD);

      if Null_Source_Buffer_Ptr (Text) then
         Write_Line ("fatal error, run-time library not installed correctly");

         if FD = Osint.Null_FD then
            Write_Line ("cannot locate file system.ads");
         else
            Write_Line ("no read access for file system.ads");
         end if;

         raise Unrecoverable_Error;
      end if;

      Get_Target_Parameters
        (System_Text  => Text,
         Source_First => 0,
         Source_Last  => Hi,
         Make_Id      => Make_Id,
         Make_SC      => Make_SC,
         Set_NOD      => Set_NOD,
         Set_NSA      => Set_NSA,
         Set_NUA      => Set_NUA,
         Set_NUP      => Set_NUP);
   end Get_Target_Parameters;

   --  Version where caller supplies system.ads text

   procedure Get_Target_Parameters
     (System_Text  : Source_Buffer_Ptr;
      Source_First : Source_Ptr;
      Source_Last  : Source_Ptr;
      Make_Id      : Make_Id_Type := null;
      Make_SC      : Make_SC_Type := null;
      Set_NOD      : Set_NOD_Type := null;
      Set_NSA      : Set_NSA_Type := null;
      Set_NUA      : Set_NUA_Type := null;
      Set_NUP      : Set_NUP_Type := null)
   is
      pragma Assert (System_Text'First = Source_First);
      pragma Assert (System_Text'Last = Source_Last);

      P : Source_Ptr;
      --  Scans source buffer containing source of system.ads

      Fatal : Boolean := False;
      --  Set True if a fatal error is detected

      Result : Boolean;
      --  Records boolean from system line

      OK : Boolean;
      --  Status result from Set_NUP/NSA/NUA call

      PR_Start : Source_Ptr;
      --  Pointer to ( following pragma Restrictions

      procedure Collect_Name;
      --  Scan a name starting at System_Text (P), and put Name in Name_Buffer,
      --  with Name_Len being length, folded to lower case. On return, P points
      --  just past the last character (which should be a right paren).

      function Looking_At (S : Source_Buffer) return Boolean;
      --  True if P points to the same text as S in System_Text

      function Looking_At_Skip (S : Source_Buffer) return Boolean;
      --  True if P points to the same text as S in System_Text,
      --  and if True, moves P forward to skip S as a side effect.

      ------------------
      -- Collect_Name --
      ------------------

      procedure Collect_Name is
      begin
         Name_Len := 0;
         loop
            if System_Text (P) in 'a' .. 'z'
              or else
                System_Text (P) = '_'
              or else
                System_Text (P) in '0' .. '9'
            then
               Name_Buffer (Name_Len + 1) := System_Text (P);

            elsif System_Text (P) in 'A' .. 'Z' then
               Name_Buffer (Name_Len + 1) :=
                 Character'Val (Character'Pos (System_Text (P)) + 32);

            else
               exit;
            end if;

            P := P + 1;
            Name_Len := Name_Len + 1;
         end loop;
      end Collect_Name;

      ----------------
      -- Looking_At --
      ----------------

      function Looking_At (S : Source_Buffer) return Boolean is
         Last : constant Source_Ptr := P + S'Length - 1;
      begin
         return Last <= System_Text'Last
           and then System_Text (P .. Last) = S;
      end Looking_At;

      ---------------------
      -- Looking_At_Skip --
      ---------------------

      function Looking_At_Skip (S : Source_Buffer) return Boolean is
         Result : constant Boolean := Looking_At (S);
      begin
         if Result then
            P := P + S'Length;
         end if;

         return Result;
      end Looking_At_Skip;

   --  Start of processing for Get_Target_Parameters

   begin
      if Parameters_Obtained then
         return;
      end if;

      Parameters_Obtained := True;
      Opt.Address_Is_Private := False;

      --  Loop through source lines

      --  Note: in the case or pragmas, we are only interested in pragmas that
      --  appear as configuration pragmas. These are left justified, so they
      --  do not have three spaces at the start. Pragmas appearing within the
      --  package (like Pure and No_Elaboration_Code_All) will have the three
      --  spaces at the start and so will be ignored.

      --  For a special exception, see processing for pragma Pure below

      P := Source_First;

      while not Looking_At ("end System;") loop
         --  Skip comments

         if Looking_At ("-") then
            goto Line_Loop_Continue;

         --  Test for type Address is private

         elsif Looking_At_Skip ("   type Address is private;") then
            Opt.Address_Is_Private := True;
            goto Line_Loop_Continue;

         --  Test for pragma Profile (Ravenscar);

         elsif Looking_At_Skip ("pragma Profile (Ravenscar);") then
            Set_Profile_Restrictions (Ravenscar);
            Opt.Task_Dispatching_Policy := 'F';
            Opt.Locking_Policy          := 'C';
            goto Line_Loop_Continue;

         --  Test for pragma Profile (Jorvik);

         elsif Looking_At_Skip ("pragma Profile (Jorvik);") then
            Set_Profile_Restrictions (Jorvik);
            Opt.Task_Dispatching_Policy := 'F';
            Opt.Locking_Policy          := 'C';
            goto Line_Loop_Continue;

         --  Test for pragma Profile (GNAT_Extended_Ravenscar);

         elsif Looking_At_Skip
           ("pragma Profile (GNAT_Extended_Ravenscar);")
         then
            Set_Profile_Restrictions (GNAT_Extended_Ravenscar);
            Opt.Task_Dispatching_Policy := 'F';
            Opt.Locking_Policy          := 'C';
            goto Line_Loop_Continue;

         --  Test for pragma Profile (GNAT_Ravenscar_EDF);

         elsif Looking_At_Skip ("pragma Profile (GNAT_Ravenscar_EDF);") then
            Set_Profile_Restrictions (GNAT_Ravenscar_EDF);
            Opt.Task_Dispatching_Policy := 'E';
            Opt.Locking_Policy          := 'C';
            goto Line_Loop_Continue;

         --  Test for pragma Profile (Restricted);

         elsif Looking_At_Skip ("pragma Profile (Restricted);") then
            Set_Profile_Restrictions (Restricted);
            goto Line_Loop_Continue;

         --  Test for pragma Restrictions

         elsif Looking_At_Skip ("pragma Restrictions (") then
            PR_Start := P - 1;

            --  Boolean restrictions

            for K in All_Boolean_Restrictions loop
               declare
                  Rname : constant String := Restriction_Id'Image (K);

               begin
                  for J in Rname'Range loop
                     if Fold_Upper (System_Text (P + Source_Ptr (J - 1)))
                                                        /= Rname (J)
                     then
                        goto Rloop_Continue;
                     end if;
                  end loop;

                  if System_Text (P + Rname'Length) = ')' then
                     Restrictions_On_Target.Set (K) := True;
                     goto Line_Loop_Continue;
                  end if;
               end;

               <<Rloop_Continue>> null;
            end loop;

            --  Restrictions taking integer parameter

            Ploop : for K in Integer_Parameter_Restrictions loop
               declare
                  Rname : constant String :=
                            All_Parameter_Restrictions'Image (K);

                  V : Natural;
                  --  Accumulates value

               begin
                  for J in Rname'Range loop
                     if Fold_Upper (System_Text (P + Source_Ptr (J - 1)))
                                                        /= Rname (J)
                     then
                        goto Ploop_Continue;
                     end if;
                  end loop;

                  if System_Text (P + Rname'Length .. P + Rname'Length + 3) =
                                                      " => "
                  then
                     P := P + Rname'Length + 4;

                     V := 0;
                     loop
                        if System_Text (P) in '0' .. '9' then
                           declare
                              pragma Unsuppress (Overflow_Check);

                           begin
                              --  Accumulate next digit

                              V := 10 * V +
                                   Character'Pos (System_Text (P)) -
                                   Character'Pos ('0');

                           exception
                              --  On overflow, we just ignore the pragma since
                              --  that is the standard handling in this case.

                              when Constraint_Error =>
                                 goto Line_Loop_Continue;
                           end;

                        elsif System_Text (P) = '_' then
                           null;

                        elsif System_Text (P) = ')' then
                           Restrictions_On_Target.Value (K) := V;
                           Restrictions_On_Target.Set (K) := True;
                           goto Line_Loop_Continue;

                        else
                           exit Ploop;
                        end if;

                        P := P + 1;
                     end loop;

                  else
                     exit Ploop;
                  end if;
               end;

               <<Ploop_Continue>> null;
            end loop Ploop;

            --  No_Dependence case

            if Looking_At_Skip ("No_Dependence => ") then
               --  Skip this processing (and simply ignore No_Dependence lines)
               --  if caller did not supply the three subprograms we need to
               --  process these lines.

               if Make_Id = null then
                  goto Line_Loop_Continue;
               end if;

               --  We have scanned out "pragma Restrictions (No_Dependence =>"

               declare
                  Unit  : Node_Id;
                  Id    : Node_Id;
                  Start : Source_Ptr;

               begin
                  Unit := Empty;

                  --  Loop through components of name, building up Unit

                  loop
                     Start := P;
                     while System_Text (P) /= '.'
                             and then
                           System_Text (P) /= ')'
                     loop
                        P := P + 1;
                     end loop;

                     Id := Make_Id (System_Text (Start .. P - 1));

                     --  If first name, just capture the identifier

                     if Unit = Empty then
                        Unit := Id;
                     else
                        Unit := Make_SC (Unit, Id);
                     end if;

                     exit when System_Text (P) = ')';
                     P := P + 1;
                  end loop;

                  Set_NOD (Unit);
                  goto Line_Loop_Continue;
               end;

            --  No_Specification_Of_Aspect case

            elsif Looking_At_Skip ("No_Specification_Of_Aspect => ") then
               --  Skip this processing (and simply ignore the pragma), if
               --  caller did not supply the subprogram we need to process
               --  such lines.

               if Set_NSA = null then
                  goto Line_Loop_Continue;
               end if;

               --  We have scanned
               --    "pragma Restrictions (No_Specification_Of_Aspect =>"

               Collect_Name;

               if System_Text (P) /= ')' then
                  goto Bad_Restrictions_Pragma;

               else
                  Set_NSA (Name_Find, OK);

                  if OK then
                     goto Line_Loop_Continue;
                  else
                     goto Bad_Restrictions_Pragma;
                  end if;
               end if;

            --  No_Use_Of_Attribute case

            elsif Looking_At_Skip ("No_Use_Of_Attribute => ") then
               --  Skip this processing (and simply ignore No_Use_Of_Attribute
               --  lines) if caller did not supply the subprogram we need to
               --  process such lines.

               if Set_NUA = null then
                  goto Line_Loop_Continue;
               end if;

               --  We have scanned
               --    "pragma Restrictions (No_Use_Of_Attribute =>"

               Collect_Name;

               if System_Text (P) /= ')' then
                  goto Bad_Restrictions_Pragma;

               else
                  Set_NUA (Name_Find, OK);

                  if OK then
                     goto Line_Loop_Continue;
                  else
                     goto Bad_Restrictions_Pragma;
                  end if;
               end if;

            --  No_Use_Of_Pragma case

            elsif Looking_At_Skip ("No_Use_Of_Pragma => ") then
               --  Skip this processing (and simply ignore No_Use_Of_Pragma
               --  lines) if caller did not supply the subprogram we need to
               --  process such lines.

               if Set_NUP = null then
                  goto Line_Loop_Continue;
               end if;

               --  We have scanned
               --    "pragma Restrictions (No_Use_Of_Pragma =>"

               Collect_Name;

               if System_Text (P) /= ')' then
                  goto Bad_Restrictions_Pragma;

               else
                  Set_NUP (Name_Find, OK);

                  if OK then
                     goto Line_Loop_Continue;
                  else
                     goto Bad_Restrictions_Pragma;
                  end if;
               end if;
            end if;

            --  Here if unrecognizable restrictions pragma form

            <<Bad_Restrictions_Pragma>>

            Set_Standard_Error;
            Write_Line
               ("fatal error: system.ads is incorrectly formatted");
            Write_Str ("unrecognized or incorrect restrictions pragma: ");

            P := PR_Start;
            loop
               exit when System_Text (P) = ASCII.LF;
               Write_Char (System_Text (P));
               exit when System_Text (P) = ')';
               P := P + 1;
            end loop;

            Write_Eol;
            Fatal := True;
            Set_Standard_Output;

         --  Test for pragma Detect_Blocking;

         elsif Looking_At_Skip ("pragma Detect_Blocking;") then
            Opt.Detect_Blocking := True;
            goto Line_Loop_Continue;

         --  Discard_Names

         elsif Looking_At_Skip ("pragma Discard_Names;") then
            Opt.Global_Discard_Names := True;
            goto Line_Loop_Continue;

         --  Locking Policy

         elsif Looking_At_Skip ("pragma Locking_Policy (") then
            Opt.Locking_Policy := System_Text (P);
            Opt.Locking_Policy_Sloc := System_Location;
            goto Line_Loop_Continue;

         --  Normalize_Scalars

         elsif Looking_At_Skip ("pragma Normalize_Scalars;") then
            Opt.Normalize_Scalars := True;
            Opt.Init_Or_Norm_Scalars := True;
            goto Line_Loop_Continue;

         --  Partition_Elaboration_Policy

         elsif Looking_At_Skip ("pragma Partition_Elaboration_Policy (") then
            Opt.Partition_Elaboration_Policy := System_Text (P);
            Opt.Partition_Elaboration_Policy_Sloc := System_Location;
            goto Line_Loop_Continue;

         --  Queuing Policy

         elsif Looking_At_Skip ("pragma Queuing_Policy (") then
            Opt.Queuing_Policy := System_Text (P);
            Opt.Queuing_Policy_Sloc := System_Location;
            goto Line_Loop_Continue;

         --  Suppress_Exception_Locations

         elsif Looking_At_Skip ("pragma Suppress_Exception_Locations;") then
            Opt.Exception_Locations_Suppressed := True;
            goto Line_Loop_Continue;

         --  Task_Dispatching Policy

         elsif Looking_At_Skip ("pragma Task_Dispatching_Policy (") then
            Opt.Task_Dispatching_Policy := System_Text (P);
            Opt.Task_Dispatching_Policy_Sloc := System_Location;
            goto Line_Loop_Continue;

         --  Allow "pragma Style_Checks (On);" and "pragma Style_Checks (Off);"
         --  to make it possible to have long "pragma Restrictions" line.

         elsif Looking_At_Skip ("pragma Style_Checks (On);") or else
           Looking_At_Skip ("pragma Style_Checks (Off);")
         then
            goto Line_Loop_Continue;

         --  No other configuration pragmas are permitted

         elsif Looking_At ("pragma ") then
            --  Special exception, we allow pragma Pure (System) appearing in
            --  column one. This is an obsolete usage which may show up in old
            --  tests with an obsolete version of system.ads, so we recognize
            --  and ignore it to make life easier in handling such tests.

            if Looking_At_Skip ("pragma Pure (System);") then
               goto Line_Loop_Continue;
            end if;

            Set_Standard_Error;
            Write_Line ("unrecognized line in system.ads: ");

            while System_Text (P) /= ')'
              and then System_Text (P) /= ASCII.LF
            loop
               Write_Char (System_Text (P));
               P := P + 1;
            end loop;

            Write_Eol;
            Set_Standard_Output;
            Fatal := True;

         --  See if we have a Run_Time_Name

         elsif Looking_At_Skip
           ("   Run_Time_Name : constant String := """)
         then
            Name_Len := 0;
            while System_Text (P) in 'A' .. 'Z'
                    or else
                  System_Text (P) in 'a' .. 'z'
                    or else
                  System_Text (P) in '0' .. '9'
                    or else
                  System_Text (P) = ' '
                    or else
                  System_Text (P) = '_'
            loop
               Add_Char_To_Name_Buffer (System_Text (P));
               P := P + 1;
            end loop;

            if System_Text (P) /= '"'
              or else System_Text (P + 1) /= ';'
              or else (System_Text (P + 2) /= ASCII.LF
                         and then
                       System_Text (P + 2) /= ASCII.CR)
            then
               Set_Standard_Error;
               Write_Line
                 ("incorrectly formatted Run_Time_Name in system.ads");
               Set_Standard_Output;
               Fatal := True;

            else
               Run_Time_Name_On_Target := Name_Enter;
            end if;

            goto Line_Loop_Continue;

         --  See if we have an Executable_Extension

         elsif Looking_At_Skip
           ("   Executable_Extension : constant String := """)
         then
            Name_Len := 0;
            while System_Text (P) /= '"'
              and then System_Text (P) /= ASCII.LF
            loop
               Add_Char_To_Name_Buffer (System_Text (P));
               P := P + 1;
            end loop;

            if System_Text (P) /= '"' or else System_Text (P + 1) /= ';' then
               Set_Standard_Error;
               Write_Line
                 ("incorrectly formatted Executable_Extension in system.ads");
               Set_Standard_Output;
               Fatal := True;

            else
               Executable_Extension_On_Target := Name_Enter;
            end if;

            goto Line_Loop_Continue;

         --  Next see if we have a configuration parameter

         else
            Config_Param_Loop : for K in Targparm_Tags loop
               if Looking_At_Skip ("   " & Targparm_Str (K).all) then
                  if Targparm_Flags (K) then
                     Set_Standard_Error;
                     Write_Line
                       ("fatal error: system.ads is incorrectly formatted");
                     Write_Str ("duplicate line for parameter: ");

                     for J in Targparm_Str (K)'Range loop
                        Write_Char (Targparm_Str (K).all (J));
                     end loop;

                     Write_Eol;
                     Set_Standard_Output;
                     Fatal := True;

                  else
                     Targparm_Flags (K) := True;
                  end if;

                  while System_Text (P) /= ':'
                     or else System_Text (P + 1) /= '='
                  loop
                     P := P + 1;
                  end loop;

                  P := P + 2;

                  while System_Text (P) = ' ' loop
                     P := P + 1;
                  end loop;

                  Result := (System_Text (P) = 'T');

                  case K is
                     when ACR => Always_Compatible_Rep_On_Target     := Result;
                     when ASD => Atomic_Sync_Default_On_Target       := Result;
                     when BDC => Backend_Divide_Checks_On_Target     := Result;
                     when BOC => Backend_Overflow_Checks_On_Target   := Result;
                     when CLA => Command_Line_Args_On_Target         := Result;
                     when CRT => Configurable_Run_Time_On_Target     := Result;
                     when D32 => Duration_32_Bits_On_Target          := Result;
                     when DEN => Denorm_On_Target                    := Result;
                     when EXS => Exit_Status_Supported_On_Target     := Result;
                     when MOV => Machine_Overflows_On_Target         := Result;
                     when MRN => Machine_Rounds_On_Target            := Result;
                     when PAS => Preallocated_Stacks_On_Target       := Result;
                     when SAG => Support_Aggregates_On_Target        := Result;
                     when SAP => Support_Atomic_Primitives_On_Target := Result;
                     when SCA => Support_Composite_Assign_On_Target  := Result;
                     when SCC => Support_Composite_Compare_On_Target := Result;
                     when SCD => Stack_Check_Default_On_Target       := Result;
                     when SCL => Stack_Check_Limits_On_Target        := Result;
                     when SCP => Stack_Check_Probes_On_Target        := Result;
                     when SLS => Support_Long_Shifts_On_Target       := Result;
                     when SSL => Suppress_Standard_Library_On_Target := Result;
                     when SNZ => Signed_Zeros_On_Target              := Result;
                     when UAM => Use_Ada_Main_Program_Name_On_Target := Result;
                     when ZCX => ZCX_By_Default_On_Target            := Result;

                     goto Line_Loop_Continue;
                  end case;

                  --  Here we are seeing a parameter we do not understand. We
                  --  simply ignore this (will happen when an old compiler is
                  --  used to compile a newer version of GNAT which does not
                  --  support the parameter).
               end if;
            end loop Config_Param_Loop;
         end if;

         --  Here after processing one line of System spec

         <<Line_Loop_Continue>>

         while P < Source_Last
           and then System_Text (P) /= CR
           and then System_Text (P) /= LF
         loop
            P := P + 1;
         end loop;

         while P < Source_Last
           and then (System_Text (P) = CR
                       or else System_Text (P) = LF)
         loop
            P := P + 1;
         end loop;

         if P >= Source_Last then
            Set_Standard_Error;
            Write_Line ("fatal error, system.ads not formatted correctly");
            Write_Line ("unexpected end of file");
            Set_Standard_Output;
            raise Unrecoverable_Error;
         end if;
      end loop;

      if Fatal then
         raise Unrecoverable_Error;
      end if;
   end Get_Target_Parameters;

   ------------------------------
   -- Set_Profile_Restrictions --
   ------------------------------

   procedure Set_Profile_Restrictions (P : Profile_Name) is
      R : Restriction_Flags  renames Profile_Info (P).Set;
      V : Restriction_Values renames Profile_Info (P).Value;
   begin
      for J in R'Range loop
         if R (J) then
            Restrictions_On_Target.Set (J) := True;

            if J in All_Parameter_Restrictions then
               Restrictions_On_Target.Value (J) := V (J);
            end if;
         end if;
      end loop;
   end Set_Profile_Restrictions;

end Targparm;
