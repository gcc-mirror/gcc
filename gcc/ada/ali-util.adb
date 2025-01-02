------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             A L I . U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Debug;   use Debug;
with Binderr; use Binderr;
with Opt;     use Opt;
with Output;  use Output;
with Osint;   use Osint;
with Scans;   use Scans;
with Fname;   use Fname;
with Scng;
with Sinput.C;
with Stringt;
with Styleg;

with System.OS_Lib; use System.OS_Lib;

package body ALI.Util is

   --  Empty procedures needed to instantiate Scng. Error procedures are
   --  empty, because we don't want to report any errors when computing
   --  a source checksum.

   procedure Post_Scan is null;

   procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr) is null;
   procedure Error_Msg_S (Msg : String) is null;
   procedure Error_Msg_SC (Msg : String) is null;
   procedure Error_Msg_SP (Msg : String) is null;

   --  Instantiation of Styleg, needed to instantiate Scng

   package Style is new Styleg
     (Error_Msg, Error_Msg_S, Error_Msg_SC, Error_Msg_SP);

   --  A Scanner is needed to get checksum of a source (procedure
   --  Get_File_Checksum).

   package Scanner is new Scng
     (Post_Scan, Error_Msg, Error_Msg_S, Error_Msg_SC, Error_Msg_SP, Style);

   type Header_Num is range 0 .. 1_000;

   function Hash (F : File_Name_Type) return Header_Num;
   --  Function used to compute hash of ALI file name

   package Interfaces is new Simple_HTable (
     Header_Num => Header_Num,
     Element    => Boolean,
     No_Element => False,
     Key        => File_Name_Type,
     Hash       => Hash,
     Equal      => "=");

   ---------------------
   -- Checksums_Match --
   ---------------------

   function Checksums_Match (Checksum1, Checksum2 : Word) return Boolean is
   begin
      return Checksum1 = Checksum2 and then Checksum1 /= Checksum_Error;
   end Checksums_Match;

   -----------------------
   -- Get_File_Checksum --
   -----------------------

   function Get_File_Checksum (Fname : File_Name_Type) return Word is
      Full_Name           : File_Name_Type;
      Source_Index        : Source_File_Index;
      Ada_Version_Current : Ada_Version_Type;
      Internal_Unit       : constant Boolean := Is_Internal_File_Name (Fname);

   begin
      Full_Name := Find_File (Fname, Osint.Source);

      --  If we cannot find the file, then return an impossible checksum,
      --  impossible because checksums have the high order bit zero, so
      --  that checksums do not match.

      if Full_Name = No_File then
         return Checksum_Error;
      end if;

      Source_Index := Sinput.C.Load_File (Get_Name_String (Full_Name));

      if Source_Index <= No_Source_File then
         return Checksum_Error;
      end if;

      Scanner.Initialize_Scanner (Source_Index);

      --  The runtime files are precompiled with an implicitly defined Ada
      --  version that we set here to improve the parsing required to compute
      --  the checksum.

      if Internal_Unit then
         Ada_Version_Current := Ada_Version;
         Ada_Version := Ada_Version_Runtime;
      end if;

      --  Scan the complete file to compute its checksum

      loop
         Scanner.Scan;
         exit when Token = Tok_EOF;
      end loop;

      --  Restore the Ada version if we changed it

      if Internal_Unit then
         Ada_Version := Ada_Version_Current;
      end if;

      return Scans.Checksum;
   end Get_File_Checksum;

   ----------
   -- Hash --
   ----------

   function Hash (F : File_Name_Type) return Header_Num is
   begin
      return Header_Num (Int (F) mod Header_Num'Range_Length);
   end Hash;

   ---------------------------
   -- Initialize_ALI_Source --
   ---------------------------

   procedure Initialize_ALI_Source is
   begin
      --  When (re)initializing ALI data structures the ALI user expects to
      --  get a fresh set of data structures. Thus we first need to erase the
      --  marks put in the name table by the previous set of ALI routine calls.
      --  This loop is empty and harmless the first time in.

      for J in Source.First .. Source.Last loop
         Set_Name_Table_Int (Source.Table (J).Sfile, 0);
         Source.Table (J).Source_Found := False;
      end loop;

      Source.Init;
      Interfaces.Reset;
   end Initialize_ALI_Source;

   ----------------------
   -- Read_Withed_ALIs --
   ----------------------

   procedure Read_Withed_ALIs (Id : ALI_Id) is
      Afile  : File_Name_Type;
      Text   : Text_Buffer_Ptr;
      Idread : ALI_Id;

   begin
      --  Process all dependent units

      for U in ALIs.Table (Id).First_Unit .. ALIs.Table (Id).Last_Unit loop
         for W in Units.Table (U).First_With .. Units.Table (U).Last_With loop
            Afile := Withs.Table (W).Afile;

            --  Only process if not a generic (Afile /= No_File) and if
            --  file has not been processed already.

            if Afile /= No_File
              and then Get_Name_Table_Int (Afile) = 0
            then
               Text := Read_Library_Info (Afile);

               --  Unless in GNATprove mode, return with an error if source
               --  cannot be found. We used to skip this check when we did not
               --  compile library generics separately, but we now always do,
               --  so there is no special case here anymore.

               if Text = null then

                  if not GNATprove_Mode then
                     Error_Msg_File_1 := Afile;
                     Error_Msg_File_2 := Withs.Table (W).Sfile;
                     Error_Msg ("{ not found, { must be compiled");
                     Set_Name_Table_Int (Afile, Int (No_Unit_Id));
                     return;
                  end if;

               else
                  --  Enter in ALIs table

                  Idread :=
                    Scan_ALI
                      (F         => Afile,
                       T         => Text,
                       Err       => False);

                  Free (Text);

                  if ALIs.Table (Idread).Compile_Errors
                    and then not GNATprove_Mode
                  then
                     Error_Msg_File_1 := Withs.Table (W).Sfile;
                     Error_Msg ("{ had errors, must be fixed, and recompiled");
                     Set_Name_Table_Int (Afile, Int (No_Unit_Id));

                  --  In GNATprove mode, object files are never generated, so
                  --  No_Object=True is not considered an error.

                  elsif ALIs.Table (Idread).No_Object
                    and then not GNATprove_Mode
                  then
                     Error_Msg_File_1 := Withs.Table (W).Sfile;
                     Error_Msg ("{ must be recompiled");
                     Set_Name_Table_Int (Afile, Int (No_Unit_Id));
                  end if;

                  --  If the Unit is an Interface to a Stand-Alone Library,
                  --  set the Interface flag in the Withs table, so that its
                  --  dependant are not considered for elaboration order.

                  if ALIs.Table (Idread).SAL_Interface then
                     Withs.Table (W).SAL_Interface := True;
                     Interface_Library_Unit := True;

                     --  Set the entry in the Interfaces hash table, so that
                     --  other units that import this unit will set the flag
                     --  in their entry in the Withs table.

                     Interfaces.Set (Afile, True);

                  else
                     --  Otherwise, recurse to get new dependents

                     Read_Withed_ALIs (Idread);
                  end if;
               end if;

            --  If the ALI file has already been processed and is an interface,
            --  set the flag in the entry of the Withs table.

            elsif Interface_Library_Unit and then Interfaces.Get (Afile) then
               Withs.Table (W).SAL_Interface := True;
            end if;
         end loop;
      end loop;
   end Read_Withed_ALIs;

   ----------------------
   -- Set_Source_Table --
   ----------------------

   procedure Set_Source_Table (A : ALI_Id) is
      F     : File_Name_Type;
      S     : Source_Id;
      Stamp : Time_Stamp_Type;

   begin
      Sdep_Loop : for D in
        ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep
      loop
         F := Sdep.Table (D).Sfile;

         if F /= No_File then

            --  If this is the first time we are seeing this source file,
            --  then make a new entry in the source table.

            if Get_Name_Table_Int (F) = 0 then
               Source.Increment_Last;
               S := Source.Last;
               Set_Name_Table_Int (F, Int (S));
               Source.Table (S).Sfile := F;
               Source.Table (S).All_Timestamps_Match := True;

               --  Initialize checksum fields

               Source.Table (S).Checksum := Sdep.Table (D).Checksum;
               Source.Table (S).All_Checksums_Match := True;

               --  In check source files mode, try to get time stamp from file

               if Opt.Check_Source_Files then
                  Stamp := Source_File_Stamp (F);

                  --  If we got the stamp, then set the stamp in the source
                  --  table entry and mark it as set from the source so that
                  --  it does not get subsequently changed.

                  if Stamp (Stamp'First) /= ' ' then
                     Source.Table (S).Stamp := Stamp;
                     Source.Table (S).Source_Found := True;
                     Source.Table (S).Stamp_File := F;

                  --  If we could not find the file, then the stamp is set
                  --  from the dependency table entry (to be possibly reset
                  --  if we find a later stamp in subsequent processing)

                  else
                     Source.Table (S).Stamp := Sdep.Table (D).Stamp;
                     Source.Table (S).Source_Found := False;
                     Source.Table (S).Stamp_File := ALIs.Table (A).Afile;

                     --  In All_Sources mode, flag error of file not found

                     if Opt.All_Sources then
                        Error_Msg_File_1 := F;
                        Error_Msg ("cannot locate {");
                     end if;
                  end if;

               --  First time for this source file, but Check_Source_Files
               --  is off, so simply initialize the stamp from the Sdep entry

               else
                  Source.Table (S).Stamp := Sdep.Table (D).Stamp;
                  Source.Table (S).Source_Found := False;
                  Source.Table (S).Stamp_File := ALIs.Table (A).Afile;
               end if;

            --  Here if this is not the first time for this source file,
            --  so that the source table entry is already constructed.

            else
               S := Source_Id (Get_Name_Table_Int (F));

               --  Update checksum flag

               if not Checksums_Match
                        (Sdep.Table (D).Checksum, Source.Table (S).Checksum)
               then
                  Source.Table (S).All_Checksums_Match := False;
               end if;

               --  Check for time stamp mismatch

               if Sdep.Table (D).Stamp /= Source.Table (S).Stamp then
                  Source.Table (S).All_Timestamps_Match := False;

                  --  When we have a time stamp mismatch, we go look for the
                  --  source file even if Check_Source_Files is false, since
                  --  if we find it, then we can use it to resolve which of the
                  --  two timestamps in the ALI files is likely to be correct.
                  --  We only look in the current directory, because when
                  --  Check_Source_Files is false, other search directories are
                  --  likely to be incorrect.

                  if not Check_Source_Files
                    and then Is_Regular_File (Get_Name_String (F))
                  then
                     Stamp := Source_File_Stamp (F);

                     if Stamp (Stamp'First) /= ' ' then
                        Source.Table (S).Stamp := Stamp;
                        Source.Table (S).Source_Found := True;
                        Source.Table (S).Stamp_File := F;
                     end if;
                  end if;

                  --  If the stamp in the source table entry was set from the
                  --  source file, then we do not change it (the stamp in the
                  --  source file is always taken as the "right" one).

                  if Source.Table (S).Source_Found then
                     null;

                  --  Otherwise, we have no source file available, so we guess
                  --  that the later of the two timestamps is the right one.
                  --  Note that this guess only affects which error messages
                  --  are issued later on, not correct functionality.

                  else
                     if Sdep.Table (D).Stamp > Source.Table (S).Stamp then
                        Source.Table (S).Stamp := Sdep.Table (D).Stamp;
                        Source.Table (S).Stamp_File := ALIs.Table (A).Afile;
                     end if;
                  end if;
               end if;
            end if;

            --  Set the checksum value in the source table

            S := Source_Id (Get_Name_Table_Int (F));
            Source.Table (S).Checksum := Sdep.Table (D).Checksum;
         end if;

      end loop Sdep_Loop;
   end Set_Source_Table;

   ----------------------
   -- Set_Source_Table --
   ----------------------

   procedure Set_Source_Table is
   begin
      for A in ALIs.First .. ALIs.Last loop
         Set_Source_Table (A);
      end loop;
   end Set_Source_Table;

   -------------------------
   -- Time_Stamp_Mismatch --
   -------------------------

   function Time_Stamp_Mismatch
     (A         : ALI_Id;
      Read_Only : Boolean := False) return File_Name_Type
   is
      Src : Source_Id;
      --  Source file Id for the current Sdep entry

   begin
      for D in ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep loop
         Src := Source_Id (Get_Name_Table_Int (Sdep.Table (D).Sfile));

         if Opt.Minimal_Recompilation
           and then Sdep.Table (D).Stamp /= Source.Table (Src).Stamp
         then
            --  If minimal recompilation is in action, replace the stamp
            --  of the source file in the table if checksums match.

            --  ??? It is probably worth updating the ALI file with a new
            --  field to avoid recomputing it each time. In any case we ensure
            --  that we don't gobble up string table space by doing a mark
            --  release around this computation.

            Stringt.Mark;

            if Checksums_Match
                 (Get_File_Checksum (Sdep.Table (D).Sfile),
                  Source.Table (Src).Checksum)
            then
               if Verbose_Mode then
                  Write_Str ("   ");
                  Write_Str (Get_Name_String (Sdep.Table (D).Sfile));
                  Write_Str (": up to date, different timestamps " &
                             "but same checksum");
                  Write_Eol;
               end if;

               Sdep.Table (D).Stamp := Source.Table (Src).Stamp;
            end if;

            Stringt.Release;
         end if;

         if not Read_Only or else Source.Table (Src).Source_Found then
            if not Source.Table (Src).Source_Found
              or else Sdep.Table (D).Stamp /= Source.Table (Src).Stamp
            then
               --  If -dt debug flag set, output time stamp found/expected

               if Source.Table (Src).Source_Found and then Debug_Flag_T then
                  Write_Str ("Source: """);
                  Get_Name_String (Sdep.Table (D).Sfile);
                  Write_Str (Name_Buffer (1 .. Name_Len));
                  Write_Line ("""");

                  Write_Str ("   time stamp expected: ");
                  Write_Line (String (Sdep.Table (D).Stamp));

                  Write_Str ("      time stamp found: ");
                  Write_Line (String (Source.Table (Src).Stamp));
               end if;

               --  Return the source file

               return Source.Table (Src).Sfile;
            end if;
         end if;
      end loop;

      return No_File;
   end Time_Stamp_Mismatch;

end ALI.Util;
