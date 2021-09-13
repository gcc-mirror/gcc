------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 F M A P                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2021, Free Software Foundation, Inc.         --
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

with Opt;    use Opt;
with Osint;  use Osint;
with Output; use Output;
with Table;
with Types;  use Types;

pragma Warnings (Off);
--  This package is used also by gnatcoll
with System.OS_Lib; use System.OS_Lib;
pragma Warnings (On);

with Unchecked_Conversion;

with GNAT.HTable;

package body Fmap is

   No_Mapping_File : Boolean := False;
   --  Set to True when the specified mapping file cannot be read in
   --  procedure Initialize, so that no attempt is made to open the mapping
   --  file in procedure Update_Mapping_File.

   Max_Buffer : constant := 1_500;
   Buffer : String (1 .. Max_Buffer);
   --  Used to buffer output when writing to a new mapping file

   Buffer_Last : Natural := 0;
   --  Index of last valid character in Buffer

   type Mapping is record
      Uname : Unit_Name_Type;
      Fname : File_Name_Type;
   end record;

   package File_Mapping is new Table.Table (
     Table_Component_Type => Mapping,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 1_000,
     Table_Increment      => 1_000,
     Table_Name           => "Fmap.File_Mapping");
   --  Mapping table to map unit names to file names

   package Path_Mapping is new Table.Table (
     Table_Component_Type => Mapping,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 1_000,
     Table_Increment      => 1_000,
     Table_Name           => "Fmap.Path_Mapping");
   --  Mapping table to map file names to path names

   type Header_Num is range 0 .. 1_000;

   function Hash (F : Unit_Name_Type) return Header_Num;
   --  Function used to compute hash of unit name

   No_Entry : constant Int := -1;
   --  Signals no entry in following table

   package Unit_Hash_Table is new GNAT.HTable.Simple_HTable (
     Header_Num => Header_Num,
     Element    => Int,
     No_Element => No_Entry,
     Key        => Unit_Name_Type,
     Hash       => Hash,
     Equal      => "=");
   --  Hash table to map unit names to file names. Used in conjunction with
   --  table File_Mapping above.

   function Hash (F : File_Name_Type) return Header_Num;
   --  Function used to compute hash of file name

   package File_Hash_Table is new GNAT.HTable.Simple_HTable (
     Header_Num => Header_Num,
     Element    => Int,
     No_Element => No_Entry,
     Key        => File_Name_Type,
     Hash       => Hash,
     Equal      => "=");
   --  Hash table to map file names to path names. Used in conjunction with
   --  table Path_Mapping above.

   Last_In_Table : Int := 0;

   package Forbidden_Names is new GNAT.HTable.Simple_HTable (
     Header_Num => Header_Num,
     Element    => Boolean,
     No_Element => False,
     Key        => File_Name_Type,
     Hash       => Hash,
     Equal      => "=");

   -----------------------------
   -- Add_Forbidden_File_Name --
   -----------------------------

   procedure Add_Forbidden_File_Name (Name : File_Name_Type) is
   begin
      Forbidden_Names.Set (Name, True);
   end Add_Forbidden_File_Name;

   ---------------------
   -- Add_To_File_Map --
   ---------------------

   procedure Add_To_File_Map
     (Unit_Name : Unit_Name_Type;
      File_Name : File_Name_Type;
      Path_Name : File_Name_Type)
   is
      Unit_Entry : constant Int := Unit_Hash_Table.Get (Unit_Name);
      File_Entry : constant Int := File_Hash_Table.Get (File_Name);
   begin
      if Unit_Entry = No_Entry or else
        File_Mapping.Table (Unit_Entry).Fname /= File_Name
      then
         File_Mapping.Increment_Last;
         Unit_Hash_Table.Set (Unit_Name, File_Mapping.Last);
         File_Mapping.Table (File_Mapping.Last) :=
           (Uname => Unit_Name, Fname => File_Name);
      end if;

      if File_Entry = No_Entry or else
        Path_Mapping.Table (File_Entry).Fname /= Path_Name
      then
         Path_Mapping.Increment_Last;
         File_Hash_Table.Set (File_Name, Path_Mapping.Last);
         Path_Mapping.Table (Path_Mapping.Last) :=
           (Uname => Unit_Name, Fname => Path_Name);
      end if;
   end Add_To_File_Map;

   ----------
   -- Hash --
   ----------

   function Hash (F : File_Name_Type) return Header_Num is
   begin
      return Header_Num (Int (F) mod Header_Num'Range_Length);
   end Hash;

   function Hash (F : Unit_Name_Type) return Header_Num is
   begin
      return Header_Num (Int (F) mod Header_Num'Range_Length);
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (File_Name : String) is
      FD  : File_Descriptor;
      Src : Source_Buffer_Ptr;
      Hi  : Source_Ptr;

      First : Source_Ptr := 1;
      Last  : Source_Ptr := 0;

      Uname : Unit_Name_Type;
      Fname : File_Name_Type;
      Pname : File_Name_Type;

      procedure Empty_Tables;
      --  Remove all entries in case of incorrect mapping file

      function Find_File_Name return File_Name_Type;
      --  Return Error_File_Name if the name buffer contains "/", otherwise
      --  call Name_Find. "/" is the path name in the mapping file to indicate
      --  that a source has been suppressed, and thus should not be found by
      --  the compiler.

      function Find_Unit_Name return Unit_Name_Type;
      --  Return the unit name in the name buffer. Return Error_Unit_Name if
      --  the name buffer contains "/".

      procedure Get_Line;
      --  Get a line from the mapping file, where a line is Src (First .. Last)

      procedure Report_Truncated;
      --  Report a warning when the mapping file is truncated
      --  (number of lines is not a multiple of 3).

      ------------------
      -- Empty_Tables --
      ------------------

      procedure Empty_Tables is
      begin
         Unit_Hash_Table.Reset;
         File_Hash_Table.Reset;
         Path_Mapping.Set_Last (0);
         File_Mapping.Set_Last (0);
         Last_In_Table := 0;
      end Empty_Tables;

      --------------------
      -- Find_File_Name --
      --------------------

      function Find_File_Name return File_Name_Type is
      begin
         if Name_Buffer (1 .. Name_Len) = "/" then

            --  A path name of "/" is the indication that the source has been
            --  "suppressed". Return Error_File_Name so that the compiler does
            --  not find the source, even if it is in the include path.

            return Error_File_Name;

         else
            return Name_Find;
         end if;
      end Find_File_Name;

      --------------------
      -- Find_Unit_Name --
      --------------------

      function Find_Unit_Name return Unit_Name_Type is
      begin
         return Unit_Name_Type (Find_File_Name);
      end Find_Unit_Name;

      --------------
      -- Get_Line --
      --------------

      procedure Get_Line is
         use ASCII;

      begin
         First := Last + 1;

         --  If not at the end of file, skip the end of line

         while First < Src'Last
           and then (Src (First) = CR
                      or else Src (First) = LF
                      or else Src (First) = EOF)
         loop
            First := First + 1;
         end loop;

         --  If not at the end of file, find the end of this new line

         if First < Src'Last and then Src (First) /= EOF then
            Last := First;

            while Last < Src'Last
              and then Src (Last + 1) /= CR
              and then Src (Last + 1) /= LF
              and then Src (Last + 1) /= EOF
            loop
               Last := Last + 1;
            end loop;

         end if;
      end Get_Line;

      ----------------------
      -- Report_Truncated --
      ----------------------

      procedure Report_Truncated is
      begin
         Write_Str ("warning: mapping file """);
         Write_Str (File_Name);
         Write_Line (""" is truncated");
      end Report_Truncated;

   --  Start of processing for Initialize

   begin
      Empty_Tables;
      Read_Source_File (Name_Enter (File_Name), 1, Hi, Src, FD, Config);

      if Null_Source_Buffer_Ptr (Src) then
         if FD = Null_FD then
            Write_Str ("warning: could not locate mapping file """);
         else
            Write_Str ("warning: no read access for mapping file """);
         end if;

         Write_Str (File_Name);
         Write_Line ("""");
         No_Mapping_File := True;

      else
         loop
            --  Get the unit name

            Get_Line;

            --  Exit if end of file has been reached

            exit when First > Last;

            if (Last < First + 2) or else (Src (Last - 1) /= '%')
              or else (Src (Last) /= 's' and then Src (Last) /= 'b')
            then
               Write_Line
                 ("warning: mapping file """ & File_Name &
                  """ is incorrectly formatted");
               Write_Line ("Line = """ & String (Src (First .. Last)) & '"');
               Empty_Tables;
               return;
            end if;

            Name_Len := Integer (Last - First + 1);
            Name_Buffer (1 .. Name_Len) := String (Src (First .. Last));
            Uname := Find_Unit_Name;

            --  Get the file name

            Get_Line;

            --  If end of line has been reached, file is truncated

            if First > Last then
               Report_Truncated;
               Empty_Tables;
               return;
            end if;

            Name_Len := Integer (Last - First + 1);
            Name_Buffer (1 .. Name_Len) := String (Src (First .. Last));
            Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
            Fname := Find_File_Name;

            --  Get the path name

            Get_Line;

            --  If end of line has been reached, file is truncated

            if First > Last then
               Report_Truncated;
               Empty_Tables;
               return;
            end if;

            Name_Len := Integer (Last - First + 1);
            Name_Buffer (1 .. Name_Len) := String (Src (First .. Last));
            Pname := Find_File_Name;

            --  Add the mappings for this unit name

            Add_To_File_Map (Uname, Fname, Pname);
         end loop;
      end if;

      --  Record the length of the two mapping tables

      Last_In_Table := File_Mapping.Last;
   end Initialize;

   ----------------------
   -- Mapped_File_Name --
   ----------------------

   function Mapped_File_Name (Unit : Unit_Name_Type) return File_Name_Type is
      The_Index : constant Int := Unit_Hash_Table.Get (Unit);

   begin
      if The_Index = No_Entry then
         return No_File;
      else
         return File_Mapping.Table (The_Index).Fname;
      end if;
   end Mapped_File_Name;

   ----------------------
   -- Mapped_Path_Name --
   ----------------------

   function Mapped_Path_Name (File : File_Name_Type) return File_Name_Type is
      Index : Int := No_Entry;

   begin
      if Forbidden_Names.Get (File) then
         return Error_File_Name;
      end if;

      Index := File_Hash_Table.Get (File);

      if Index = No_Entry then
         return No_File;
      else
         return Path_Mapping.Table (Index).Fname;
      end if;
   end Mapped_Path_Name;

   ------------------
   -- Reset_Tables --
   ------------------

   procedure Reset_Tables is
   begin
      File_Mapping.Init;
      Path_Mapping.Init;
      Unit_Hash_Table.Reset;
      File_Hash_Table.Reset;
      Forbidden_Names.Reset;
      Last_In_Table := 0;
   end Reset_Tables;

   -------------------------
   -- Update_Mapping_File --
   -------------------------

   procedure Update_Mapping_File (File_Name : String) is
      File    : File_Descriptor;
      N_Bytes : Integer;

      File_Entry : Int;

      Status : Boolean;
      --  For the call to Close

      procedure Put_Line (Name : Name_Id);
      --  Put Name as a line in the Mapping File

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (Name : Name_Id) is
      begin
         Get_Name_String (Name);

         --  If the Buffer is full, write it to the file

         if Buffer_Last + Name_Len + 1 > Buffer'Last then
            N_Bytes := Write (File, Buffer (1)'Address, Buffer_Last);

            if N_Bytes < Buffer_Last then
               Fail ("disk full");
            end if;

            Buffer_Last := 0;
         end if;

         --  Add the line to the Buffer

         Buffer (Buffer_Last + 1 .. Buffer_Last + Name_Len) :=
           Name_Buffer (1 .. Name_Len);
         Buffer_Last := Buffer_Last + Name_Len + 1;
         Buffer (Buffer_Last) := ASCII.LF;
      end Put_Line;

   --  Start of processing for Update_Mapping_File

   begin
      --  If the mapping file could not be read, then it will not be possible
      --  to update it.

      if No_Mapping_File then
         return;
      end if;
      --  Only Update if there are new entries in the mappings

      if Last_In_Table < File_Mapping.Last then

         File := Open_Read_Write (Name => File_Name, Fmode => Binary);

         if File /= Invalid_FD then
            if Last_In_Table > 0 then
               Lseek (File, 0, Seek_End);
            end if;

            for Unit in Last_In_Table + 1 .. File_Mapping.Last loop
               Put_Line (Name_Id (File_Mapping.Table (Unit).Uname));
               Put_Line (Name_Id (File_Mapping.Table (Unit).Fname));
               File_Entry :=
                 File_Hash_Table.Get (File_Mapping.Table (Unit).Fname);
               Put_Line (Name_Id (Path_Mapping.Table (File_Entry).Fname));
            end loop;

            --  Before closing the file, write the buffer to the file. It is
            --  guaranteed that the Buffer is not empty, because Put_Line has
            --  been called at least 3 times, and after a call to Put_Line, the
            --  Buffer is not empty.

            N_Bytes := Write (File, Buffer (1)'Address, Buffer_Last);

            if N_Bytes < Buffer_Last then
               Fail ("disk full");
            end if;

            Close (File, Status);

            if not Status then
               Fail ("disk full");
            end if;

         elsif not Quiet_Output then
            Write_Str ("warning: could not open mapping file """);
            Write_Str (File_Name);
            Write_Line (""" for update");
         end if;

      end if;
   end Update_Mapping_File;

end Fmap;
