------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N P U T . D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2019, Free Software Foundation, Inc.         --
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

with Debug;         use Debug;
with Osint;         use Osint;
with Osint.C;       use Osint.C;
with Output;        use Output;
with System.OS_Lib; use System.OS_Lib;

package body Sinput.D is

   Dfile : Source_File_Index;
   --  Index of currently active debug source file

   ------------------------
   -- Close_Debug_Source --
   ------------------------

   procedure Close_Debug_Source is
      FD   : File_Descriptor;
      SFR  : Source_File_Record renames Source_File.Table (Dfile);
      Src  : Source_Buffer_Ptr;
   begin
      Trim_Lines_Table (Dfile);
      Close_Debug_File;

      --  Now we need to read the file that we wrote and store it in memory for
      --  subsequent access.

      Read_Source_File
        (SFR.Full_Debug_Name, SFR.Source_First, SFR.Source_Last, Src, FD);
      SFR.Source_Text := Src;
      pragma Assert (SFR.Source_Text'First = SFR.Source_First);
      pragma Assert (SFR.Source_Text'Last = SFR.Source_Last);
   end Close_Debug_Source;

   -------------------------
   -- Create_Debug_Source --
   -------------------------

   procedure Create_Debug_Source
     (Source : Source_File_Index;
      Loc    : out Source_Ptr)
   is
   begin
      Loc :=
        ((Source_File.Table (Source_File.Last).Source_Last + Source_Align) /
           Source_Align) * Source_Align;
      Source_File.Append (Source_File.Table (Source));
      Dfile := Source_File.Last;

      declare
         S : Source_File_Record renames Source_File.Table (Dfile);

      begin
         S.Index             := Dfile;
         S.Full_Debug_Name   := Create_Debug_File (S.File_Name);
         S.Debug_Source_Name := Strip_Directory (S.Full_Debug_Name);
         S.Source_Text       := null;
         S.Source_First      := Loc;
         S.Source_Last       := Loc;
         S.Lines_Table       := null;
         S.Last_Source_Line  := 1;

         --  Allocate lines table, guess that it needs to be three times bigger
         --  than the original source (in number of lines).

         Alloc_Line_Tables
           (S, Int (Source_File.Table (Source).Last_Source_Line * 3));
         S.Lines_Table (1) := Loc;

         if Debug_Flag_L then
            Write_Str ("Sinput.D.Create_Debug_Source: created source ");
            Write_Int (Int (Dfile));
            Write_Str (" for ");
            Write_Str (Get_Name_String (S.Full_Debug_Name));
            Write_Line ("");
         end if;
      end;
   end Create_Debug_Source;

   ----------------------
   -- Write_Debug_Line --
   ----------------------

   procedure Write_Debug_Line (Str : String; Loc : in out Source_Ptr) is
      S : Source_File_Record renames Source_File.Table (Dfile);

   begin
      --  Ignore write request if null line at start of file

      if Str'Length = 0 and then Loc = S.Source_First then
         return;

      --  Here we write the line, compute the source location for the following
      --  line, allocate its table entry, and update the source record entry.

      else
         Write_Debug_Info (Str (Str'First .. Str'Last - 1));
         Loc := Loc - 1 + Source_Ptr (Str'Length + Debug_File_Eol_Length);
         Add_Line_Tables_Entry (S, Loc);
         S.Source_Last := Loc;
         Set_Source_File_Index_Table (Dfile);
      end if;
   end Write_Debug_Line;

end Sinput.D;
