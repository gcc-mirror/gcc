------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               B L D - I O                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--        Copyright (C) 2002-2003 Free Software Foundation, Inc.            --
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

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Osint;

package body Bld.IO is

   use Ada;

   Initial_Number_Of_Lines : constant := 100;
   Initial_Length_Of_Line  : constant := 50;

   type Line is record
      Length     : Natural := 0;
      Value      : String_Access;
      Suppressed : Boolean := False;
   end record;
   --  One line of a Makefile.
   --  Length is the position of the last column in the line.
   --  Suppressed is set to True by procedure Suppress.

   type Line_Array is array (Positive range <>) of Line;

   type Buffer is access Line_Array;

   procedure Free is new Ada.Unchecked_Deallocation (Line_Array, Buffer);

   Lines : Buffer := new Line_Array (1 .. Initial_Number_Of_Lines);
   --  The lines of a Makefile

   Current : Positive := 1;
   --  Position of the last line in the Makefile

   File : Text_IO.File_Type;
   --  The current Makefile

   type File_Name_Data;
   type File_Name_Ref is access File_Name_Data;

   type File_Name_Data is record
      Value : String_Access;
      Next : File_Name_Ref;
   end record;
   --  Used to record the names of all Makefiles created, so that we may delete
   --  them if necessary.

   File_Names : File_Name_Ref;
   --  List of all the Makefiles created so far.

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Flush;
      Text_IO.Close (File);

   exception
      when X : others =>
         Text_IO.Put_Line (Exceptions.Exception_Message (X));
         Osint.Fail ("cannot close a Makefile");
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create (File_Name : String) is
   begin
      Text_IO.Create (File, Text_IO.Out_File, File_Name);
      Current := 1;
      Lines (1).Length := 0;
      Lines (1).Suppressed := False;
      File_Names :=
        new File_Name_Data'(Value => new String'(File_Name),
                            Next  => File_Names);
   exception
      when X : others =>
         Text_IO.Put_Line (Exceptions.Exception_Message (X));
         Osint.Fail ("cannot create """ & File_Name & '"');
   end Create;

   ----------------
   -- Delete_All --
   ----------------

   procedure Delete_All is
      Success : Boolean;
   begin
      if Text_IO.Is_Open (File) then
         Text_IO.Delete (File);
         File_Names := File_Names.Next;
      end if;

      while File_Names /= null loop
         Delete_File (File_Names.Value.all, Success);
         File_Names := File_Names.Next;
      end loop;
   end Delete_All;

   -----------
   -- Flush --
   -----------

   procedure Flush is
      Last : Natural;
   begin
      if Lines (Current).Length /= 0 then
         Osint.Fail ("INTERNAL ERROR: flushing before end of line: """ &
                 Lines (Current).Value
                   (1 .. Lines (Current).Length));
      end if;

      for J in 1 .. Current - 1 loop
         if not Lines (J).Suppressed then
            Last := Lines (J).Length;

            --  The last character of a line cannot be a back slash ('\'),
            --  otherwise make has a problem. The only real place were it
            --  should happen is for directory names on Windows, and then
            --  this terminal back slash is not needed.

            if Last > 0 and then Lines (J).Value (Last) = '\' then
               Last := Last - 1;
            end if;

            Text_IO.Put_Line (File, Lines (J).Value (1 .. Last));
         end if;
      end loop;

      Current := 1;
      Lines (1).Length := 0;
      Lines (1).Suppressed := False;
   end Flush;

   ----------
   -- Mark --
   ----------

   procedure Mark (Pos : out Position) is
   begin
      if Lines (Current).Length /= 0 then
         Osint.Fail ("INTERNAL ERROR: marking before end of line: """ &
                 Lines (Current).Value
                   (1 .. Lines (Current).Length));
      end if;

      Pos := (Value => Current);
   end Mark;

   ------------------
   -- Name_Of_File --
   ------------------

   function Name_Of_File return String is
   begin
      return Text_IO.Name (File);
   end Name_Of_File;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Current := Current + 1;

      if Current > Lines'Last then
         declare
            New_Lines : constant Buffer :=
                          new Line_Array (1 .. 2 * Lines'Last);

         begin
            New_Lines (1 .. Lines'Last) := Lines.all;
            Free (Lines);
            Lines := New_Lines;
         end;
      end if;

      Lines (Current).Length := 0;
      Lines (Current).Suppressed := False;

      --  Allocate a new line, if necessary

      if Lines (Current).Value = null then
         Lines (Current).Value := new String (1 .. Initial_Length_Of_Line);
      end if;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (S : String) is
      Length : constant Natural := Lines (Current).Length;

   begin
      if Length + S'Length > Lines (Current).Value'Length then
         declare
            New_Line   : String_Access;
            New_Length : Positive := 2 * Lines (Current).Value'Length;
         begin
            while Length + S'Length > New_Length loop
               New_Length := 2 * New_Length;
            end loop;

            New_Line := new String (1 .. New_Length);
            New_Line (1 .. Length) := Lines (Current).Value (1 .. Length);
            Free (Lines (Current).Value);
            Lines (Current).Value := New_Line;
         end;
      end if;

      Lines (Current).Value (Length + 1 .. Length + S'Length) := S;
      Lines (Current).Length := Length + S'Length;
   end Put;

   -------------
   -- Release --
   -------------

   procedure Release (Pos : Position) is
   begin
      if Lines (Current).Length /= 0 then
         Osint.Fail ("INTERNAL ERROR: releasing before end of line: """ &
                 Lines (Current).Value
                   (1 .. Lines (Current).Length));
      end if;

      if Pos.Value > Current then
         Osint.Fail ("INTERNAL ERROR: releasing ahead of current position");
      end if;

      Current := Pos.Value;
      Lines (Current).Length := 0;
   end Release;

   --------------
   -- Suppress --
   --------------

   procedure Suppress (Pos : Position) is
   begin
      if Pos.Value >= Current then
         Osint.Fail ("INTERNAL ERROR: suppressing ahead of current position");
      end if;

      Lines (Pos.Value).Suppressed := True;
   end Suppress;

begin
   --  Allocate the first line.
   --  The other ones are allocated by New_Line.

   Lines (1).Value := new String (1 .. Initial_Length_Of_Line);
end Bld.IO;
