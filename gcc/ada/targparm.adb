------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                             T A R G P A R M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1999-2001 Free Software Foundation, Inc.          --
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

with Namet;    use Namet;
with Output;   use Output;
with Sinput;   use Sinput;
with Sinput.L; use Sinput.L;
with Types;    use Types;

package body Targparm is

   type Targparm_Tags is
     (AAM,  --   AAMP;
      BDC,  --   Backend_Divide_Checks;
      BOC,  --   Backend_Overflow_Checks;
      CLA,  --   Command_Line_Args;
      DEN,  --   Denorm;
      DSP,  --   Functions_Return_By_DSP;
      FEL,  --   Frontend_Layout;
      FFO,  --   Fractional_Fixed_Ops
      HIM,  --   High_Integrity_Mode;
      LSI,  --   Long_Shifts_Inlined;
      MOV,  --   Machine_Overflows;
      MRN,  --   Machine_Rounds;
      SCD,  --   Stack_Check_Default;
      SCP,  --   Stack_Check_Probes;
      SNZ,  --   Signed_Zeros;
      UAM,  --   Use_Ada_Main_Program_Name;
      VMS,  --   OpenVMS;
      ZCD,  --   ZCX_By_Default;
      ZCG,  --   GCC_ZCX_Support;
      ZCF); --   Front_End_ZCX_Support;

   Targparm_Flags : array (Targparm_Tags) of Boolean := (others => False);
   --  Flag is set True if corresponding parameter is scanned

   AAM_Str : aliased constant Source_Buffer := "AAMP";
   BDC_Str : aliased constant Source_Buffer := "Backend_Divide_Checks";
   BOC_Str : aliased constant Source_Buffer := "Backend_Overflow_Checks";
   CLA_Str : aliased constant Source_Buffer := "Command_Line_Args";
   DEN_Str : aliased constant Source_Buffer := "Denorm";
   DSP_Str : aliased constant Source_Buffer := "Functions_Return_By_DSP";
   FEL_Str : aliased constant Source_Buffer := "Frontend_Layout";
   FFO_Str : aliased constant Source_Buffer := "Fractional_Fixed_Ops";
   HIM_Str : aliased constant Source_Buffer := "High_Integrity_Mode";
   LSI_Str : aliased constant Source_Buffer := "Long_Shifts_Inlined";
   MOV_Str : aliased constant Source_Buffer := "Machine_Overflows";
   MRN_Str : aliased constant Source_Buffer := "Machine_Rounds";
   SCD_Str : aliased constant Source_Buffer := "Stack_Check_Default";
   SCP_Str : aliased constant Source_Buffer := "Stack_Check_Probes";
   SNZ_Str : aliased constant Source_Buffer := "Signed_Zeros";
   UAM_Str : aliased constant Source_Buffer := "Use_Ada_Main_Program_Name";
   VMS_Str : aliased constant Source_Buffer := "OpenVMS";
   ZCD_Str : aliased constant Source_Buffer := "ZCX_By_Default";
   ZCG_Str : aliased constant Source_Buffer := "GCC_ZCX_Support";
   ZCF_Str : aliased constant Source_Buffer := "Front_End_ZCX_Support";

   type Buffer_Ptr is access constant Source_Buffer;
   Targparm_Str : array (Targparm_Tags) of Buffer_Ptr :=
     (AAM_Str'Access,
      BDC_Str'Access,
      BOC_Str'Access,
      CLA_Str'Access,
      DEN_Str'Access,
      DSP_Str'Access,
      FEL_Str'Access,
      FFO_Str'Access,
      HIM_Str'Access,
      LSI_Str'Access,
      MOV_Str'Access,
      MRN_Str'Access,
      SCD_Str'Access,
      SCP_Str'Access,
      SNZ_Str'Access,
      UAM_Str'Access,
      VMS_Str'Access,
      ZCD_Str'Access,
      ZCG_Str'Access,
      ZCF_Str'Access);

   ---------------------------
   -- Get_Target_Parameters --
   ---------------------------

   procedure Get_Target_Parameters is
      use ASCII;

      S : Source_File_Index;
      N : Name_Id;
      T : Source_Buffer_Ptr;
      P : Source_Ptr;
      Z : Source_Ptr;

      Fatal : Boolean := False;
      --  Set True if a fatal error is detected

      Result : Boolean;
      --  Records boolean from system line

   begin
      Name_Buffer (1 .. 10) := "system.ads";
      Name_Len := 10;
      N := Name_Find;
      S := Load_Source_File (N);

      if S = No_Source_File then
         Write_Line ("fatal error, run-time library not installed correctly");
         Write_Str ("cannot locate file ");
         Write_Line (Name_Buffer (1 .. Name_Len));
         raise Unrecoverable_Error;

      --  This must always be the first source file read, and we have defined
      --  a constant Types.System_Source_File_Index as 1 to reflect this.

      else
         pragma Assert (S = System_Source_File_Index);
         null;
      end if;

      P := Source_First (S);
      Z := Source_Last  (S);
      T := Source_Text  (S);

      while T (P .. P + 10) /= "end System;" loop

         for K in Targparm_Tags loop
            if T (P + 3 .. P + 2 + Targparm_Str (K)'Length) =
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

               while T (P) /= ':' or else T (P + 1) /= '=' loop
                  P := P + 1;
               end loop;

               P := P + 2;

               while T (P) = ' ' loop
                  P := P + 1;
               end loop;

               Result := (T (P) = 'T');

               case K is
                  when AAM => AAMP_On_Target                      := Result;
                  when BDC => Backend_Divide_Checks_On_Target     := Result;
                  when BOC => Backend_Overflow_Checks_On_Target   := Result;
                  when CLA => Command_Line_Args_On_Target         := Result;
                  when DEN => Denorm_On_Target                    := Result;
                  when DSP => Functions_Return_By_DSP_On_Target   := Result;
                  when FEL => Frontend_Layout_On_Target           := Result;
                  when FFO => Fractional_Fixed_Ops_On_Target      := Result;
                  when HIM => High_Integrity_Mode_On_Target       := Result;
                  when LSI => Long_Shifts_Inlined_On_Target       := Result;
                  when MOV => Machine_Overflows_On_Target         := Result;
                  when MRN => Machine_Rounds_On_Target            := Result;
                  when SCD => Stack_Check_Default_On_Target       := Result;
                  when SCP => Stack_Check_Probes_On_Target        := Result;
                  when SNZ => Signed_Zeros_On_Target              := Result;
                  when UAM => Use_Ada_Main_Program_Name_On_Target := Result;
                  when VMS => OpenVMS_On_Target                   := Result;
                  when ZCD => ZCX_By_Default_On_Target            := Result;
                  when ZCG => GCC_ZCX_Support_On_Target           := Result;
                  when ZCF => Front_End_ZCX_Support_On_Target     := Result;
               end case;

               exit;
            end if;
         end loop;

         while T (P) /= CR and then T (P) /= LF loop
            P := P + 1;
            exit when P >= Z;
         end loop;

         while T (P) = CR or else T (P) = LF loop
            P := P + 1;
            exit when P >= Z;
         end loop;

         if P >= Z then
            Set_Standard_Error;
            Write_Line ("fatal error, system.ads not formatted correctly");
            Set_Standard_Output;
            raise Unrecoverable_Error;
         end if;
      end loop;

      for K in Targparm_Tags loop
         if not Targparm_Flags (K) then
            Set_Standard_Error;
            Write_Line
              ("fatal error: system.ads is incorrectly formatted");
            Write_Str ("missing line for parameter: ");

            for J in Targparm_Str (K)'Range loop
               Write_Char (Targparm_Str (K).all (J));
            end loop;

            Write_Eol;
            Set_Standard_Output;
            Fatal := True;
         end if;
      end loop;

      if Fatal then
         raise Unrecoverable_Error;
      end if;
   end Get_Target_Parameters;

end Targparm;
