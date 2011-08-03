------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                            A L F A _ T E S T                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011, Free Software Foundation, Inc.            --
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

--  This utility program is used to test proper operation of the Get_ALFA and
--  Put_ALFA units. To run it, compile any source file with switch -gnatd.E or
--  -gnatd.F to get an ALI file file.ALI containing ALFA information. Then run
--  this utility using:

--     ALFA_Test file.ali

--  This test will read the ALFA information from the ALI file, and use
--  Get_ALFA to store this in binary form in the internal tables in ALFA. Then
--  Put_ALFA is used to write the information from these tables back into text
--  form. This output is compared with the original ALFA information in the ALI
--  file and the two should be identical. If not an error message is output.

with Get_ALFA;
with Put_ALFA;

with ALFA;  use ALFA;
with Types; use Types;

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO;

procedure ALFA_Test is
   Infile    : File_Type;
   Outfile_1 : File_Type;
   Outfile_2 : File_Type;
   C         : Character;

   Stop : exception;
   --  Terminate execution

   use ASCII;

begin
   if Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: alfa_test FILE.ali");
      raise Stop;
   end if;

   Create (Outfile_1, Out_File, "log1");
   Create (Outfile_2, Out_File, "log2");
   Open   (Infile,    In_File,  Argument (1));

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

      --  Subprograms used by Get_ALFA (these also copy the output to Outfile_1
      --  for later comparison with the output generated by Put_ALFA).

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

      --  Subprograms used by Put_ALFA, which write information to Outfile_2

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

      --  Local instantiations of Put_ALFA and Get_ALFA

      procedure Get_ALFA_Info is new Get_ALFA;
      procedure Put_ALFA_Info is new Put_ALFA;

   --  Start of processing for Process

   begin
      --  Loop to skip till first 'F' line

      loop
         C := Get_Char (Infile);

         if C = EOF then
            Ada.Text_IO.Put_Line
              (Argument (1) & ": no SCO found, recompile with -gnateS");
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

      --  Read ALFA information to internal ALFA tables, also copying ALFA info
      --  to Outfile_1.

      Initialize_ALFA_Tables;
      Get_ALFA_Info;

      --  Write ALFA information from internal ALFA tables to Outfile_2

      Put_ALFA_Info;

      --  Junk blank line (see comment at end of Lib.Writ)

      Write_Info_Terminate;

      --  Now Outfile_1 and Outfile_2 should be identical

      Compare_Files : declare
         Line : Natural;
         Col  : Natural;
         C1   : Character;
         C2   : Character;

      begin
         Reset (Outfile_1, In_File);
         Reset (Outfile_2, In_File);

         --  Loop to compare the two files

         Line := 1;
         Col  := 1;
         loop
            C1 := Get_Char (Outfile_1);
            C2 := Get_Char (Outfile_2);
            exit when C1 = EOF or else C1 /= C2;

            if C1 = LF then
               Line := Line + 1;
               Col  := 1;
            else
               Col := Col + 1;
            end if;
         end loop;

         --  If we reached the end of file, then the files were identical,
         --  otherwise, we have a failure in the comparison.

         if C1 = EOF then
            --  Success: exit silently

            null;

         else
            Ada.Text_IO.Put_Line
              (Argument (1) & ": failure, files log1 and log2 differ at line"
               & Line'Img & " column" & Col'Img);
         end if;
      end Compare_Files;
   end Process;

exception
   when Stop =>
      null;
end ALFA_Test;
