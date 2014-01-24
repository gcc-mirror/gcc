------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                             T A R G P A R M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2013, Free Software Foundation, Inc.         --
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

with Csets;    use Csets;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;

package body Targparm is
   use ASCII;

   Parameters_Obtained : Boolean := False;
   --  Set True after first call to Get_Target_Parameters. Used to avoid
   --  reading system.ads more than once, since it cannot change.

   --  The following array defines a tag name for each entry

   type Targparm_Tags is
     (AAM,  --   AAMP
      ACR,  --   Always_Compatible_Rep
      ASD,  --   Atomic_Sync_Default
      BDC,  --   Backend_Divide_Checks
      BOC,  --   Backend_Overflow_Checks
      CLA,  --   Command_Line_Args
      CLI,  --   CLI (.NET)
      CRT,  --   Configurable_Run_Times
      D32,  --   Duration_32_Bits
      DEN,  --   Denorm
      EXS,  --   Exit_Status_Supported
      FEL,  --   Frontend_Layout
      FFO,  --   Fractional_Fixed_Ops
      JVM,  --   JVM
      MOV,  --   Machine_Overflows
      MRN,  --   Machine_Rounds
      PAS,  --   Preallocated_Stacks
      RTX,  --   RTX_RTSS_Kernel_Module
      SAG,  --   Support_Aggregates
      SAP,  --   Support_Atomic_Primitives
      SCA,  --   Support_Composite_Assign
      SCC,  --   Support_Composite_Compare
      SCD,  --   Stack_Check_Default
      SCL,  --   Stack_Check_Limits
      SCP,  --   Stack_Check_Probes
      SHE,  --   Short_Enums
      SLS,  --   Support_Long_Shifts
      SNZ,  --   Signed_Zeros
      SSL,  --   Suppress_Standard_Library
      UAM,  --   Use_Ada_Main_Program_Name
      VMS,  --   OpenVMS
      VXF,  --   VAX Float
      ZCD); --   ZCX_By_Default

   Targparm_Flags : array (Targparm_Tags) of Boolean := (others => False);
   --  Flag is set True if corresponding parameter is scanned

   --  The following list of string constants gives the parameter names

   AAM_Str : aliased constant Source_Buffer := "AAMP";
   ACR_Str : aliased constant Source_Buffer := "Always_Compatible_Rep";
   ASD_Str : aliased constant Source_Buffer := "Atomic_Sync_Default";
   BDC_Str : aliased constant Source_Buffer := "Backend_Divide_Checks";
   BOC_Str : aliased constant Source_Buffer := "Backend_Overflow_Checks";
   CLA_Str : aliased constant Source_Buffer := "Command_Line_Args";
   CLI_Str : aliased constant Source_Buffer := "CLI";
   CRT_Str : aliased constant Source_Buffer := "Configurable_Run_Time";
   D32_Str : aliased constant Source_Buffer := "Duration_32_Bits";
   DEN_Str : aliased constant Source_Buffer := "Denorm";
   EXS_Str : aliased constant Source_Buffer := "Exit_Status_Supported";
   FEL_Str : aliased constant Source_Buffer := "Frontend_Layout";
   FFO_Str : aliased constant Source_Buffer := "Fractional_Fixed_Ops";
   JVM_Str : aliased constant Source_Buffer := "JVM";
   MOV_Str : aliased constant Source_Buffer := "Machine_Overflows";
   MRN_Str : aliased constant Source_Buffer := "Machine_Rounds";
   PAS_Str : aliased constant Source_Buffer := "Preallocated_Stacks";
   RTX_Str : aliased constant Source_Buffer := "RTX_RTSS_Kernel_Module";
   SAG_Str : aliased constant Source_Buffer := "Support_Aggregates";
   SAP_Str : aliased constant Source_Buffer := "Support_Atomic_Primitives";
   SCA_Str : aliased constant Source_Buffer := "Support_Composite_Assign";
   SCC_Str : aliased constant Source_Buffer := "Support_Composite_Compare";
   SCD_Str : aliased constant Source_Buffer := "Stack_Check_Default";
   SCL_Str : aliased constant Source_Buffer := "Stack_Check_Limits";
   SCP_Str : aliased constant Source_Buffer := "Stack_Check_Probes";
   SHE_Str : aliased constant Source_Buffer := "Short_Enums";
   SLS_Str : aliased constant Source_Buffer := "Support_Long_Shifts";
   SNZ_Str : aliased constant Source_Buffer := "Signed_Zeros";
   SSL_Str : aliased constant Source_Buffer := "Suppress_Standard_Library";
   UAM_Str : aliased constant Source_Buffer := "Use_Ada_Main_Program_Name";
   VMS_Str : aliased constant Source_Buffer := "OpenVMS";
   VXF_Str : aliased constant Source_Buffer := "VAX_Float";
   ZCD_Str : aliased constant Source_Buffer := "ZCX_By_Default";

   --  The following defines a set of pointers to the above strings,
   --  indexed by the tag values.

   type Buffer_Ptr is access constant Source_Buffer;
   Targparm_Str : constant array (Targparm_Tags) of Buffer_Ptr :=
     (AAM_Str'Access,
      ACR_Str'Access,
      ASD_Str'Access,
      BDC_Str'Access,
      BOC_Str'Access,
      CLA_Str'Access,
      CLI_Str'Access,
      CRT_Str'Access,
      D32_Str'Access,
      DEN_Str'Access,
      EXS_Str'Access,
      FEL_Str'Access,
      FFO_Str'Access,
      JVM_Str'Access,
      MOV_Str'Access,
      MRN_Str'Access,
      PAS_Str'Access,
      RTX_Str'Access,
      SAG_Str'Access,
      SAP_Str'Access,
      SCA_Str'Access,
      SCC_Str'Access,
      SCD_Str'Access,
      SCL_Str'Access,
      SCP_Str'Access,
      SHE_Str'Access,
      SLS_Str'Access,
      SNZ_Str'Access,
      SSL_Str'Access,
      UAM_Str'Access,
      VMS_Str'Access,
      VXF_Str'Access,
      ZCD_Str'Access);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Set_Profile_Restrictions (P : Profile_Name);
   --  Set Restrictions_On_Target for the given profile

   ---------------------------
   -- Get_Target_Parameters --
   ---------------------------

   --  Version which reads in system.ads

   procedure Get_Target_Parameters is
      Text : Source_Buffer_Ptr;
      Hi   : Source_Ptr;

   begin
      if Parameters_Obtained then
         return;
      end if;

      Name_Buffer (1 .. 10) := "system.ads";
      Name_Len := 10;

      Read_Source_File (Name_Find, Lo => 0, Hi => Hi, Src => Text);

      if Text = null then
         Write_Line ("fatal error, run-time library not installed correctly");
         Write_Line ("cannot locate file system.ads");
         raise Unrecoverable_Error;
      end if;

      Get_Target_Parameters
        (System_Text  => Text,
         Source_First => 0,
         Source_Last  => Hi);
   end Get_Target_Parameters;

   --  Version where caller supplies system.ads text

   procedure Get_Target_Parameters
     (System_Text  : Source_Buffer_Ptr;
      Source_First : Source_Ptr;
      Source_Last  : Source_Ptr)
   is
      P : Source_Ptr;
      --  Scans source buffer containing source of system.ads

      Fatal : Boolean := False;
      --  Set True if a fatal error is detected

      Result : Boolean;
      --  Records boolean from system line

   begin
      if Parameters_Obtained then
         return;
      else
         Parameters_Obtained := True;
      end if;

      Opt.Address_Is_Private := False;

      P := Source_First;
      Line_Loop : while System_Text (P .. P + 10) /= "end System;" loop

         --  Skip comments quickly

         if System_Text (P) = '-' then
            goto Line_Loop_Continue;

         --  Test for type Address is private

         elsif System_Text (P .. P + 26) = "   type Address is private;" then
            Opt.Address_Is_Private := True;
            P := P + 26;
            goto Line_Loop_Continue;

         --  Test for pragma Profile (Ravenscar);

         elsif System_Text (P .. P + 26) =
                 "pragma Profile (Ravenscar);"
         then
            Set_Profile_Restrictions (Ravenscar);
            Opt.Task_Dispatching_Policy := 'F';
            Opt.Locking_Policy          := 'C';
            P := P + 27;
            goto Line_Loop_Continue;

         --  Test for pragma Profile (Restricted);

         elsif System_Text (P .. P + 27) =
                 "pragma Profile (Restricted);"
         then
            Set_Profile_Restrictions (Restricted);
            P := P + 28;
            goto Line_Loop_Continue;

         --  Test for pragma Restrictions

         elsif System_Text (P .. P + 20) = "pragma Restrictions (" then
            P := P + 21;

            Rloop : for K in All_Boolean_Restrictions loop
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

            <<Rloop_Continue>>
               null;
            end loop Rloop;

            Ploop : for K in All_Parameter_Restrictions loop
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

            <<Ploop_Continue>>
               null;
            end loop Ploop;

            Set_Standard_Error;
            Write_Line
               ("fatal error: system.ads is incorrectly formatted");
            Write_Str ("unrecognized or incorrect restrictions pragma: ");

            while System_Text (P) /= ')'
                    and then
                  System_Text (P) /= ASCII.LF
            loop
               Write_Char (System_Text (P));
               P := P + 1;
            end loop;

            Write_Eol;
            Fatal := True;
            Set_Standard_Output;

         --  Test for pragma Detect_Blocking;

         elsif System_Text (P .. P + 22) = "pragma Detect_Blocking;" then
            P := P + 23;
            Opt.Detect_Blocking := True;
            goto Line_Loop_Continue;

         --  Discard_Names

         elsif System_Text (P .. P + 20) = "pragma Discard_Names;" then
            P := P + 21;
            Opt.Global_Discard_Names := True;
            goto Line_Loop_Continue;

         --  Locking Policy

         elsif System_Text (P .. P + 22) = "pragma Locking_Policy (" then
            P := P + 23;
            Opt.Locking_Policy := System_Text (P);
            Opt.Locking_Policy_Sloc := System_Location;
            goto Line_Loop_Continue;

         --  Normalize_Scalars

         elsif System_Text (P .. P + 24) = "pragma Normalize_Scalars;" then
            P := P + 25;
            Opt.Normalize_Scalars := True;
            Opt.Init_Or_Norm_Scalars := True;
            goto Line_Loop_Continue;

         --  Partition_Elaboration_Policy

         elsif System_Text (P .. P + 36) =
                 "pragma Partition_Elaboration_Policy ("
         then
            P := P + 37;
            Opt.Partition_Elaboration_Policy := System_Text (P);
            Opt.Partition_Elaboration_Policy_Sloc := System_Location;
            goto Line_Loop_Continue;

         --  Polling (On)

         elsif System_Text (P .. P + 19) = "pragma Polling (On);" then
            P := P + 20;
            Opt.Polling_Required := True;
            goto Line_Loop_Continue;

         --  Ignore pragma Pure (System)

         elsif System_Text (P .. P + 20) = "pragma Pure (System);" then
            P := P + 21;
            goto Line_Loop_Continue;

         --  Queuing Policy

         elsif System_Text (P .. P + 22) = "pragma Queuing_Policy (" then
            P := P + 23;
            Opt.Queuing_Policy := System_Text (P);
            Opt.Queuing_Policy_Sloc := System_Location;
            goto Line_Loop_Continue;

         --  Suppress_Exception_Locations

         elsif System_Text (P .. P + 35) =
                                   "pragma Suppress_Exception_Locations;"
         then
            P := P + 36;
            Opt.Exception_Locations_Suppressed := True;
            goto Line_Loop_Continue;

         --  Task_Dispatching Policy

         elsif System_Text (P .. P + 31) =
                                   "pragma Task_Dispatching_Policy ("
         then
            P := P + 32;
            Opt.Task_Dispatching_Policy := System_Text (P);
            Opt.Task_Dispatching_Policy_Sloc := System_Location;
            goto Line_Loop_Continue;

         --  No other pragmas are permitted

         elsif System_Text (P .. P + 6) = "pragma " then
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

         elsif System_Text (P .. P + 38) =
                  "   Run_Time_Name : constant String := """
         then
            P := P + 39;

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

         elsif System_Text (P .. P + 45) =
                  "   Executable_Extension : constant String := """
         then
            P := P + 46;

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
               if System_Text (P + 3 .. P + 2 + Targparm_Str (K)'Length) =
                                                      Targparm_Str (K).all
               then
                  P := P + 3 + Targparm_Str (K)'Length;

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
                     when AAM => AAMP_On_Target                      := Result;
                     when ACR => Always_Compatible_Rep_On_Target     := Result;
                     when ASD => Atomic_Sync_Default_On_Target       := Result;
                     when BDC => Backend_Divide_Checks_On_Target     := Result;
                     when BOC => Backend_Overflow_Checks_On_Target   := Result;
                     when CLA => Command_Line_Args_On_Target         := Result;
                     when CLI =>
                        if Result then
                           VM_Target := CLI_Target;
                           Tagged_Type_Expansion := False;
                        end if;
                        --  This is wrong, this processing should be done in
                        --  Gnat1drv.Adjust_Global_Switches. It is not the
                        --  right level for targparm to know about tagged
                        --  type extension???

                     when CRT => Configurable_Run_Time_On_Target     := Result;
                     when D32 => Duration_32_Bits_On_Target          := Result;
                     when DEN => Denorm_On_Target                    := Result;
                     when EXS => Exit_Status_Supported_On_Target     := Result;
                     when FEL => Frontend_Layout_On_Target           := Result;
                     when FFO => Fractional_Fixed_Ops_On_Target      := Result;

                     when JVM =>
                        if Result then
                           VM_Target := JVM_Target;
                           Tagged_Type_Expansion := False;
                        end if;
                        --  This is wrong, this processing should be done in
                        --  Gnat1drv.Adjust_Global_Switches. It is not the
                        --  right level for targparm to know about tagged
                        --  type extension???

                     when MOV => Machine_Overflows_On_Target         := Result;
                     when MRN => Machine_Rounds_On_Target            := Result;
                     when PAS => Preallocated_Stacks_On_Target       := Result;
                     when RTX => RTX_RTSS_Kernel_Module_On_Target    := Result;
                     when SAG => Support_Aggregates_On_Target        := Result;
                     when SAP => Support_Atomic_Primitives_On_Target := Result;
                     when SCA => Support_Composite_Assign_On_Target  := Result;
                     when SCC => Support_Composite_Compare_On_Target := Result;
                     when SCD => Stack_Check_Default_On_Target       := Result;
                     when SCL => Stack_Check_Limits_On_Target        := Result;
                     when SCP => Stack_Check_Probes_On_Target        := Result;
                     when SHE => Short_Enums_On_Target               := Result;
                     when SLS => Support_Long_Shifts_On_Target       := Result;
                     when SSL => Suppress_Standard_Library_On_Target := Result;
                     when SNZ => Signed_Zeros_On_Target              := Result;
                     when UAM => Use_Ada_Main_Program_Name_On_Target := Result;
                     when VMS => OpenVMS_On_Target                   := Result;
                     when VXF => VAX_Float_On_Target                 := Result;
                     when ZCD => ZCX_By_Default_On_Target            := Result;

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

         while System_Text (P) /= CR and then System_Text (P) /= LF loop
            P := P + 1;
            exit when P >= Source_Last;
         end loop;

         while System_Text (P) = CR or else System_Text (P) = LF loop
            P := P + 1;
            exit when P >= Source_Last;
         end loop;

         if P >= Source_Last then
            Set_Standard_Error;
            Write_Line ("fatal error, system.ads not formatted correctly");
            Write_Line ("unexpected end of file");
            Set_Standard_Output;
            raise Unrecoverable_Error;
         end if;
      end loop Line_Loop;

      --  Now that OpenVMS_On_Target has been given its definitive value,
      --  change the multi-unit index character from '~' to '$' for OpenVMS.

      if OpenVMS_On_Target then
         Multi_Unit_Index_Character := '$';
      end if;

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
