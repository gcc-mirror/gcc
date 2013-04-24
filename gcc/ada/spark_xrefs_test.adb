------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                     S P A R K _ X R E F S _ T E S T                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2013, Free Software Foundation, Inc.         --
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

--  This utility program is used to test proper operation of the
--  Get_SPARK_Xrefs and Put_SPARK_Xrefs units. To run it, compile any source
--  file with switch -gnatd.E or -gnatd.F to get an ALI file file.ALI
--  containing SPARK information. Then run this utility using:

--     spark_xrefs_test file.ali

--  This test will read the SPARK cross-reference information from the ALI
--  file, and use Get_SPARK_Xrefs to store this in binary form in the internal
--  tables in SPARK_Xrefs. Then Put_SPARK_Xrefs is used to write the
--  information from these tables back into text form. This output is compared
--  with the original SPARK cross-reference information in the ALI file and the
--  two should be identical. If not an error message is output.

with Get_SPARK_Xrefs;
with Put_SPARK_Xrefs;

with SPARK_Xrefs;           use SPARK_Xrefs;
with Types;                 use Types;

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO;

with GNAT.OS_Lib;           use GNAT.OS_Lib;

procedure SPARK_Xrefs_Test is
   Infile    : File_Type;
   Name1     : String_Access;
   Outfile_1 : File_Type;
   Name2     : String_Access;
   Outfile_2 : File_Type;
   C         : Character;

   Stop : exception;
   --  Terminate execution

   Diff_Exec   : constant String_Access := Locate_Exec_On_Path ("diff");
   Diff_Result : Integer;

   use ASCII;

begin
   if Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: spark_xrefs_test FILE.ali");
      raise Stop;
   end if;

   Name1 := new String'(Argument (1) & ".1");
   Name2 := new String'(Argument (1) & ".2");

   Open   (Infile,    In_File,  Argument (1));
   Create (Outfile_1, Out_File, Name1.all);
   Create (Outfile_2, Out_File, Name2.all);

   --  Read input file till we get to first 'F' line

   Process : declare
      Output_Col : Positive := 1;

      function Get_Char (F : File_Type) return Character;
      --  Read one character from specified  file

      procedure Put_Char (F : File_Type; C : Character);
      --  Write one character to specified file

      function Get_Output_Col return Positive;
      --  Return current column in output file, where each line starts at
      --  column 1 and terminate with LF, and HT is at columns 1, 9, etc.
      --  All output is supposed to be carried through Put_Char.

      --------------
      -- Get_Char --
      --------------

      function Get_Char (F : File_Type) return Character is
         Item : Stream_Element_Array (1 .. 1);
         Last : Stream_Element_Offset;

      begin
         Read (F, Item, Last);

         if Last /= 1 then
            return Types.EOF;
         else
            return Character'Val (Item (1));
         end if;
      end Get_Char;

      --------------------
      -- Get_Output_Col --
      --------------------

      function Get_Output_Col return Positive is
      begin
         return Output_Col;
      end Get_Output_Col;

      --------------
      -- Put_Char --
      --------------

      procedure Put_Char (F : File_Type; C : Character) is
         Item : Stream_Element_Array (1 .. 1);

      begin
         if C /= CR and then C /= EOF then
            if C = LF then
               Output_Col := 1;
            elsif C = HT then
               Output_Col := ((Output_Col + 6) / 8) * 8 + 1;
            else
               Output_Col := Output_Col + 1;
            end if;

            Item (1) := Character'Pos (C);
            Write (F, Item);
         end if;
      end Put_Char;

      --  Subprograms used by Get_SPARK_Xrefs (these also copy the output to
      --  Outfile_1 for later comparison with the output generated by
      --  Put_SPARK_Xrefs).

      function  Getc  return Character;
      function  Nextc return Character;
      procedure Skipc;

      ----------
      -- Getc --
      ----------

      function Getc  return Character is
         C : Character;
      begin
         C := Get_Char (Infile);
         Put_Char (Outfile_1, C);
         return C;
      end Getc;

      -----------
      -- Nextc --
      -----------

      function Nextc return Character is
         C : Character;

      begin
         C := Get_Char (Infile);

         if C /= EOF then
            Set_Index (Infile, Index (Infile) - 1);
         end if;

         return C;
      end Nextc;

      -----------
      -- Skipc --
      -----------

      procedure Skipc is
         C : Character;
         pragma Unreferenced (C);
      begin
         C := Getc;
      end Skipc;

      --  Subprograms used by Put_SPARK_Xrefs, which write information to
      --  Outfile_2.

      function Write_Info_Col return Positive;
      procedure Write_Info_Char (C : Character);
      procedure Write_Info_Initiate (Key : Character);
      procedure Write_Info_Nat (N : Nat);
      procedure Write_Info_Terminate;

      --------------------
      -- Write_Info_Col --
      --------------------

      function Write_Info_Col return Positive is
      begin
         return Get_Output_Col;
      end Write_Info_Col;

      ---------------------
      -- Write_Info_Char --
      ---------------------

      procedure Write_Info_Char (C : Character) is
      begin
         Put_Char (Outfile_2, C);
      end Write_Info_Char;

      -------------------------
      -- Write_Info_Initiate --
      -------------------------

      procedure Write_Info_Initiate (Key : Character) is
      begin
         Write_Info_Char (Key);
      end Write_Info_Initiate;

      --------------------
      -- Write_Info_Nat --
      --------------------

      procedure Write_Info_Nat (N : Nat) is
      begin
         if N > 9 then
            Write_Info_Nat (N / 10);
         end if;

         Write_Info_Char (Character'Val (48 + N mod 10));
      end Write_Info_Nat;

      --------------------------
      -- Write_Info_Terminate --
      --------------------------

      procedure Write_Info_Terminate is
      begin
         Write_Info_Char (LF);
      end Write_Info_Terminate;

      --  Local instantiations of Put_SPARK_Xrefs and Get_SPARK_Xrefs

      procedure Get_SPARK_Xrefs_Info is new Get_SPARK_Xrefs;
      procedure Put_SPARK_Xrefs_Info is new Put_SPARK_Xrefs;

   --  Start of processing for Process

   begin
      --  Loop to skip till first 'F' line

      loop
         C := Get_Char (Infile);

         if C = EOF then
            raise Stop;

         elsif C = LF or else C = CR then
            loop
               C := Get_Char (Infile);
               exit when C /= LF and then C /= CR;
            end loop;

            exit when C = 'F';
         end if;
      end loop;

      --  Position back to initial 'F' of first 'F' line

      Set_Index (Infile, Index (Infile) - 1);

      --  Read SPARK cross-reference information to internal SPARK tables, also
      --  copying SPARK xrefs info to Outfile_1.

      Initialize_SPARK_Tables;
      Get_SPARK_Xrefs_Info;

      --  Write SPARK cross-reference information from internal SPARK tables to
      --  Outfile_2.

      Put_SPARK_Xrefs_Info;

      --  Junk blank line (see comment at end of Lib.Writ)

      Write_Info_Terminate;

      --  Flush to disk

      Close (Outfile_1);
      Close (Outfile_2);

      --  Now Outfile_1 and Outfile_2 should be identical

      Diff_Result :=
        Spawn (Diff_Exec.all,
               Argument_String_To_List
                 ("-u " & Name1.all & " " & Name2.all).all);

      if Diff_Result /= 0 then
         Ada.Text_IO.Put_Line ("diff(1) exit status" & Diff_Result'Img);
      end if;

      OS_Exit (Diff_Result);

   end Process;

exception
   when Stop =>
      null;
end SPARK_Xrefs_Test;
