------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 F M A P                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--            Copyright (C) 2001, Free Software Foundation, Inc.            --
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

with Namet;  use Namet;
with Osint;  use Osint;
with Output; use Output;
with Table;

with Unchecked_Conversion;

with GNAT.HTable;

package body Fmap is

   subtype Big_String is String (Positive);
   type Big_String_Ptr is access all Big_String;

   function To_Big_String_Ptr is new Unchecked_Conversion
     (Source_Buffer_Ptr, Big_String_Ptr);

   package File_Mapping is new Table.Table (
     Table_Component_Type => File_Name_Type,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 1_000,
     Table_Increment      => 1_000,
     Table_Name           => "Fmap.File_Mapping");
   --  Mapping table to map unit names to file names.

   package Path_Mapping is new Table.Table (
     Table_Component_Type => File_Name_Type,
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

   package File_Hash_Table is new GNAT.HTable.Simple_HTable (
     Header_Num => Header_Num,
     Element    => Int,
     No_Element => No_Entry,
     Key        => File_Name_Type,
     Hash       => Hash,
     Equal      => "=");
   --  Hash table to map file names to path names. Used in conjunction with
   --  table Path_Mapping above.

   ---------------------
   -- Add_To_File_Map --
   ---------------------

   procedure Add_To_File_Map
     (Unit_Name : Unit_Name_Type;
      File_Name : File_Name_Type;
      Path_Name : File_Name_Type)
   is
   begin
      File_Mapping.Increment_Last;
      Unit_Hash_Table.Set (Unit_Name, File_Mapping.Last);
      File_Mapping.Table (File_Mapping.Last) := File_Name;
      Path_Mapping.Increment_Last;
      File_Hash_Table.Set (File_Name, Path_Mapping.Last);
      Path_Mapping.Table (Path_Mapping.Last) := Path_Name;
   end Add_To_File_Map;

   ----------
   -- Hash --
   ----------

   function Hash (F : Unit_Name_Type) return Header_Num is
   begin
      return Header_Num (Int (F) rem Header_Num'Range_Length);
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (File_Name : String) is
      Src : Source_Buffer_Ptr;
      Hi  : Source_Ptr;
      BS  : Big_String_Ptr;
      SP  : String_Ptr;

      Deb : Positive := 1;
      Fin : Natural  := 0;

      Uname : Unit_Name_Type;
      Fname : Name_Id;
      Pname : Name_Id;

      procedure Empty_Tables;
      --  Remove all entries in case of incorrect mapping file

      procedure Get_Line;
      --  Get a line from the mapping file

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
      end Empty_Tables;

      --------------
      -- Get_Line --
      --------------

      procedure Get_Line is
         use ASCII;

      begin
         Deb := Fin + 1;

         --  If not at the end of file, skip the end of line

         while Deb < SP'Last
           and then (SP (Deb) = CR
                     or else SP (Deb) = LF
                     or else SP (Deb) = EOF)
         loop
            Deb := Deb + 1;
         end loop;

         --  If not at the end of line, find the end of this new line

         if Deb < SP'Last and then SP (Deb) /= EOF then
            Fin := Deb;

            while Fin < SP'Last
              and then SP (Fin + 1) /= CR
              and then SP (Fin + 1) /= LF
              and then SP (Fin + 1) /= EOF
            loop
               Fin := Fin + 1;
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

   --  Start of procedure Initialize

   begin
      Name_Len := File_Name'Length;
      Name_Buffer (1 .. Name_Len) := File_Name;
      Read_Source_File (Name_Enter, 0, Hi, Src, Config);

      if Src = null then
         Write_Str ("warning: could not read mapping file """);
         Write_Str (File_Name);
         Write_Line ("""");

      else
         BS := To_Big_String_Ptr (Src);
         SP := BS (1 .. Natural (Hi))'Unrestricted_Access;

         loop
            --  Get the unit name

            Get_Line;

            --  Exit if end of file has been reached

            exit when Deb > Fin;

            pragma Assert (Fin >= Deb + 2);
            pragma Assert (SP (Fin - 1) = '%');
            pragma Assert (SP (Fin) = 's' or else SP (Fin) = 'b');

            Name_Len := Fin - Deb + 1;
            Name_Buffer (1 .. Name_Len) := SP (Deb .. Fin);
            Uname := Name_Find;

            --  Get the file name

            Get_Line;

            --  If end of line has been reached, file is truncated

            if Deb > Fin then
               Report_Truncated;
               Empty_Tables;
               return;
            end if;

            Name_Len := Fin - Deb + 1;
            Name_Buffer (1 .. Name_Len) := SP (Deb .. Fin);
            Fname := Name_Find;

            --  Get the path name

            Get_Line;

            --  If end of line has been reached, file is truncated

            if Deb > Fin then
               Report_Truncated;
               Empty_Tables;
               return;
            end if;

            Name_Len := Fin - Deb + 1;
            Name_Buffer (1 .. Name_Len) := SP (Deb .. Fin);
            Pname := Name_Find;

            --  Check for duplicate entries

            if Unit_Hash_Table.Get (Uname) /= No_Entry then
               Write_Str ("warning: duplicate entry """);
               Write_Str (Get_Name_String (Uname));
               Write_Str (""" in mapping file """);
               Write_Str (File_Name);
               Write_Line ("""");
               Empty_Tables;
               return;
            end if;

            if File_Hash_Table.Get (Fname) /= No_Entry then
               Write_Str ("warning: duplicate entry """);
               Write_Str (Get_Name_String (Fname));
               Write_Str (""" in mapping file """);
               Write_Str (File_Name);
               Write_Line ("""");
               Empty_Tables;
               return;
            end if;

            --  Add the mappings for this unit name

            Add_To_File_Map (Uname, Fname, Pname);
         end loop;
      end if;
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
         return File_Mapping.Table (The_Index);
      end if;
   end Mapped_File_Name;

   ----------------------
   -- Mapped_Path_Name --
   ----------------------

   function Mapped_Path_Name (File : File_Name_Type) return File_Name_Type is
      Index : Int := No_Entry;

   begin
      Index := File_Hash_Table.Get (File);

      if Index = No_Entry then
         return No_File;
      else
         return Path_Mapping.Table (Index);
      end if;
   end Mapped_Path_Name;

end Fmap;
