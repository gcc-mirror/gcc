------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N P U T . D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 2001, Free Software Foundation, Inc.              --
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

with Osint;     use Osint;
with Osint.C;   use Osint.C;

package body Sinput.D is

   Dfile : Source_File_Index;
   --  Index of currently active debug source file

   ------------------------
   -- Close_Debug_Source --
   ------------------------

   procedure Close_Debug_Source is
      S    : Source_File_Record renames Source_File.Table (Dfile);
      Src  : Source_Buffer_Ptr;

   begin
      Trim_Lines_Table (Dfile);
      Close_Debug_File;

      --  Now we need to read the file that we wrote and store it
      --  in memory for subsequent access.

      Read_Source_File
        (S.Debug_Source_Name, S.Source_First, S.Source_Last, Src);
      S.Source_Text := Src;
   end Close_Debug_Source;

   -------------------------
   -- Create_Debug_Source --
   -------------------------

   procedure Create_Debug_Source
     (Source : Source_File_Index;
      Loc    : out Source_Ptr)
   is
   begin
      Loc := Source_File.Table (Source_File.Last).Source_Last + 1;
      Source_File.Increment_Last;
      Dfile := Source_File.Last;

      declare
         S : Source_File_Record renames Source_File.Table (Dfile);

      begin
         S := Source_File.Table (Source);
         S.Debug_Source_Name := Create_Debug_File (S.File_Name);
         S.Source_First      := Loc;
         S.Source_Last       := Loc;
         S.Lines_Table       := null;
         S.Last_Source_Line  := 1;

         --  Allocate lines table, guess that it needs to be three times
         --  bigger than the original source (in number of lines).

         Alloc_Line_Tables
           (S, Int (Source_File.Table (Source).Last_Source_Line * 3));
         S.Lines_Table (1) := Loc;
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

      --  Here we write the line, and update the source record entry

      else
         Write_Debug_Info (Str (Str'First .. Str'Last - 1));
         Add_Line_Tables_Entry (S, Loc);
         Loc := Loc - 1 + Source_Ptr (Str'Length + Debug_File_Eol_Length);
         S.Source_Last := Loc;
      end if;
   end Write_Debug_Line;

end Sinput.D;
