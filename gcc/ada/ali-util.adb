------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             A L I . U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

with Binderr; use Binderr;
with Namet;   use Namet;
with Opt;     use Opt;
with Osint;   use Osint;

with System.CRC32;

package body ALI.Util is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Accumulate_Checksum (C : Character; Csum : in out Word);
   pragma Inline (Accumulate_Checksum);
   --  This routine accumulates the checksum given character C. During the
   --  scanning of a source file, this routine is called with every character
   --  in the source, excluding blanks, and all control characters (except
   --  that ESC is included in the checksum). Upper case letters not in string
   --  literals are folded by the caller. See Sinput spec for the documentation
   --  of the checksum algorithm. Note: checksum values are only used if we
   --  generate code, so it is not necessary to worry about making the right
   --  sequence of calls in any error situation.

   procedure Initialize_Checksum (Csum : out Word);
   --  Sets initial value of Csum before any calls to Accumulate_Checksum

   -------------------------
   -- Accumulate_Checksum --
   -------------------------

   procedure Accumulate_Checksum (C : Character; Csum : in out Word) is
   begin
      System.CRC32.Update (System.CRC32.CRC32 (Csum), C);
   end Accumulate_Checksum;

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

   function Get_File_Checksum (Fname : Name_Id) return Word is
      Src  : Source_Buffer_Ptr;
      Hi   : Source_Ptr;
      Csum : Word;
      Ptr  : Source_Ptr;

      Bad : exception;
      --  Raised if file not found, or file format error

      use ASCII;
      --  Make control characters visible

      procedure Free_Source;
      --  Free source file buffer

      procedure Free_Source is
         procedure free (Arg : Source_Buffer_Ptr);
         pragma Import (C, free, "free");

      begin
         free (Src);
      end Free_Source;

   --  Start of processing for Get_File_Checksum

   begin
      Read_Source_File (Fname, 0, Hi, Src);

      --  If we cannot find the file, then return an impossible checksum,
      --  impossible becaues checksums have the high order bit zero, so
      --  that checksums do not match.

      if Src = null then
         raise Bad;
      end if;

      Initialize_Checksum (Csum);
      Ptr := 0;

      loop
         case Src (Ptr) is

            --  Spaces and formatting information are ignored in checksum

            when ' ' | CR | LF | VT | FF | HT =>
               Ptr := Ptr + 1;

            --  EOF is ignored unless it is the last character

            when EOF =>
               if Ptr = Hi then
                  Free_Source;
                  return Csum;
               else
                  Ptr := Ptr + 1;
               end if;

            --  Non-blank characters that are included in the checksum

            when '#' | '&' | '*' | ':' | '(' | ',' | '.' | '=' | '>' |
                 '<' | ')' | '/' | ';' | '|' | '!' | '+' | '_' |
                 '0' .. '9' | 'a' .. 'z'
            =>
               Accumulate_Checksum (Src (Ptr), Csum);
               Ptr := Ptr + 1;

            --  Upper case letters, fold to lower case

            when 'A' .. 'Z' =>
               Accumulate_Checksum
                 (Character'Val (Character'Pos (Src (Ptr)) + 32), Csum);
               Ptr := Ptr + 1;

            --  Left bracket, really should do wide character thing here,
            --  but for now, don't bother.

            when '[' =>
               raise Bad;

            --  Minus, could be comment

            when '-' =>
               if Src (Ptr + 1) = '-' then
                  Ptr := Ptr + 2;

                  while Src (Ptr) >= ' ' or else Src (Ptr) = HT loop
                     Ptr := Ptr + 1;
                  end loop;

               else
                  Accumulate_Checksum ('-', Csum);
                  Ptr := Ptr + 1;
               end if;

            --  String delimited by double quote

            when '"' =>
               Accumulate_Checksum ('"', Csum);

               loop
                  Ptr := Ptr + 1;
                  exit when Src (Ptr) = '"';

                  if Src (Ptr) < ' ' then
                     raise Bad;
                  end if;

                  Accumulate_Checksum (Src (Ptr), Csum);
               end loop;

               Accumulate_Checksum ('"', Csum);
               Ptr := Ptr + 1;

            --  String delimited by percent

            when '%' =>
               Accumulate_Checksum ('%', Csum);

               loop
                  Ptr := Ptr + 1;
                  exit when Src (Ptr) = '%';

                  if Src (Ptr) < ' ' then
                     raise Bad;
                  end if;

                  Accumulate_Checksum (Src (Ptr), Csum);
               end loop;

               Accumulate_Checksum ('%', Csum);
               Ptr := Ptr + 1;

            --  Quote, could be character constant

            when ''' =>
               Accumulate_Checksum (''', Csum);

               if Src (Ptr + 2) = ''' then
                  Accumulate_Checksum (Src (Ptr + 1), Csum);
                  Accumulate_Checksum (''', Csum);
                  Ptr := Ptr + 3;

               --  Otherwise assume attribute char. We should deal with wide
               --  character cases here, but that's hard, so forget it.

               else
                  Ptr := Ptr + 1;
               end if;

            --  Upper half character, more to be done here, we should worry
            --  about folding Latin-1, folding other character sets, and
            --  dealing with the nasty case of upper half wide encoding.

            when Upper_Half_Character =>
               Accumulate_Checksum (Src (Ptr), Csum);
               Ptr := Ptr + 1;

            --  Escape character, we should do the wide character thing here,
            --  but for now, do not bother.

            when ESC =>
               raise Bad;

            --  Invalid control characters

            when NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS  | SO  |
                 SI  | DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN |
                 EM  | FS  | GS  | RS  | US  | DEL
            =>
               raise Bad;

            --  Invalid graphic characters

            when '$' | '?' | '@' | '`' | '\' |
                 '^' | '~' | ']' | '{' | '}'
            =>
               raise Bad;

         end case;
      end loop;

   exception
      when Bad =>
         Free_Source;
         return Checksum_Error;

   end Get_File_Checksum;

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
         Set_Name_Table_Info (Source.Table (J).Sfile, 0);
         Source.Table (J).Source_Found := False;
      end loop;

      Source.Init;
   end Initialize_ALI_Source;

   -------------------------
   -- Initialize_Checksum --
   -------------------------

   procedure Initialize_Checksum (Csum : out Word) is
   begin
      System.CRC32.Initialize (System.CRC32.CRC32 (Csum));
   end Initialize_Checksum;

   --------------
   -- Read_ALI --
   --------------

   procedure Read_ALI (Id : ALI_Id) is
      Afile  : File_Name_Type;
      Text   : Text_Buffer_Ptr;
      Idread : ALI_Id;

   begin
      for I in ALIs.Table (Id).First_Unit .. ALIs.Table (Id).Last_Unit loop
         for J in Units.Table (I).First_With .. Units.Table (I).Last_With loop

            Afile := Withs.Table (J).Afile;

            --  Only process if not a generic (Afile /= No_File) and if
            --  file has not been processed already.

            if Afile /= No_File and then Get_Name_Table_Info (Afile) = 0 then

               Text := Read_Library_Info (Afile);

               if Text = null then
                  Error_Msg_Name_1 := Afile;
                  Error_Msg_Name_2 := Withs.Table (J).Sfile;
                  Error_Msg ("% not found, % must be compiled");
                  Set_Name_Table_Info (Afile, Int (No_Unit_Id));
                  return;
               end if;

               Idread :=
                 Scan_ALI
                   (F         => Afile,
                    T         => Text,
                    Ignore_ED => Force_RM_Elaboration_Order,
                    Err       => False);

               Free (Text);

               if ALIs.Table (Idread).Compile_Errors then
                  Error_Msg_Name_1 := Withs.Table (J).Sfile;
                  Error_Msg ("% had errors, must be fixed, and recompiled");
                  Set_Name_Table_Info (Afile, Int (No_Unit_Id));

               elsif ALIs.Table (Idread).No_Object then
                  Error_Msg_Name_1 := Withs.Table (J).Sfile;
                  Error_Msg ("% must be recompiled");
                  Set_Name_Table_Info (Afile, Int (No_Unit_Id));
               end if;

               --  Recurse to get new dependents

               Read_ALI (Idread);
            end if;
         end loop;
      end loop;

   end Read_ALI;

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

         --  If this is the first time we are seeing this source file,
         --  then make a new entry in the source table.

         if Get_Name_Table_Info (F) = 0 then
            Source.Increment_Last;
            S := Source.Last;
            Set_Name_Table_Info (F, Int (S));
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

               --  If we could not find the file, then the stamp is set
               --  from the dependency table entry (to be possibly reset
               --  if we find a later stamp in subsequent processing)

               else
                  Source.Table (S).Stamp := Sdep.Table (D).Stamp;
                  Source.Table (S).Source_Found := False;

                  --  In All_Sources mode, flag error of file not found

                  if Opt.All_Sources then
                     Error_Msg_Name_1 := F;
                     Error_Msg ("cannot locate %");
                  end if;
               end if;

            --  First time for this source file, but Check_Source_Files
            --  is off, so simply initialize the stamp from the Sdep entry

            else
               Source.Table (S).Source_Found := False;
               Source.Table (S).Stamp := Sdep.Table (D).Stamp;
            end if;

         --  Here if this is not the first time for this source file,
         --  so that the source table entry is already constructed.

         else
            S := Source_Id (Get_Name_Table_Info (F));

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

               if not Check_Source_Files then
                  Stamp := Source_File_Stamp (F);

                  if Stamp (Stamp'First) /= ' ' then
                     Source.Table (S).Stamp := Stamp;
                     Source.Table (S).Source_Found := True;
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
                  end if;
               end if;
            end if;
         end if;

         --  Set the checksum value in the source table

         S := Source_Id (Get_Name_Table_Info (F));
         Source.Table (S).Checksum := Sdep.Table (D).Checksum;

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

   function Time_Stamp_Mismatch (A : ALI_Id) return File_Name_Type is
      Src : Source_Id;
      --  Source file Id for the current Sdep entry

   begin
      for D in ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep loop
         Src := Source_Id (Get_Name_Table_Info (Sdep.Table (D).Sfile));

         if Opt.Minimal_Recompilation
           and then Sdep.Table (D).Stamp /= Source.Table (Src).Stamp
         then

            --  If minimal recompilation is in action, replace the stamp
            --  of the source file in the table if checksums match.

            --  ??? It is probably worth updating the ALI file with a new
            --  field to avoid recomputing it each time.

            if Checksums_Match
                 (Get_File_Checksum (Sdep.Table (D).Sfile),
                  Source.Table (Src).Checksum)
            then
               Sdep.Table (D).Stamp := Source.Table (Src).Stamp;
            end if;

         end if;

         if not Source.Table (Src).Source_Found
           or else Sdep.Table (D).Stamp /= Source.Table (Src).Stamp
         then
            return Source.Table (Src).Sfile;
         end if;
      end loop;

      return No_File;

   end Time_Stamp_Mismatch;

end ALI.Util;
