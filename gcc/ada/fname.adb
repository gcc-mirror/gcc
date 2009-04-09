------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                F N A M E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Alloc;
with Hostparm; use Hostparm;
with Table;
with Types;    use Types;

package body Fname is

   -----------------------------
   -- Dummy Table Definitions --
   -----------------------------

   --  The following table was used in old versions of the compiler. We retain
   --  the declarations here for compatibility with old tree files. The new
   --  version of the compiler does not use this table, and will write out a
   --  dummy empty table for Tree_Write.

   type SFN_Entry is record
      U : Unit_Name_Type;
      F : File_Name_Type;
   end record;

   package SFN_Table is new Table.Table (
     Table_Component_Type => SFN_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.SFN_Table_Initial,
     Table_Increment      => Alloc.SFN_Table_Increment,
     Table_Name           => "Fname_Dummy_Table");

   ---------------------------
   -- Is_Internal_File_Name --
   ---------------------------

   function Is_Internal_File_Name
     (Fname              : File_Name_Type;
      Renamings_Included : Boolean := True) return Boolean
   is
   begin
      if Is_Predefined_File_Name (Fname, Renamings_Included) then
         return True;

      --  Once Is_Predefined_File_Name has been called and returns False,
      --  Name_Buffer contains Fname and Name_Len is set to 8.

      elsif Name_Buffer (1 .. 2) = "g-"
        or else Name_Buffer (1 .. 8) = "gnat    "
      then
         return True;

      elsif OpenVMS
        and then
          (Name_Buffer (1 .. 4) = "dec-"
             or else Name_Buffer (1 .. 8) = "dec     ")
      then
         return True;

      else
         return False;
      end if;
   end Is_Internal_File_Name;

   -----------------------------
   -- Is_Predefined_File_Name --
   -----------------------------

   --  This should really be a test of unit name, given the possibility of
   --  pragma Source_File_Name setting arbitrary file names for any files???

   --  Once Is_Predefined_File_Name has been called and returns False,
   --  Name_Buffer contains Fname and Name_Len is set to 8. This is used
   --  only by Is_Internal_File_Name, and is not part of the official
   --  external interface of this function.

   function Is_Predefined_File_Name
     (Fname              : File_Name_Type;
      Renamings_Included : Boolean := True) return Boolean
   is
   begin
      Get_Name_String (Fname);
      return Is_Predefined_File_Name (Renamings_Included);
   end Is_Predefined_File_Name;

   function Is_Predefined_File_Name
     (Renamings_Included : Boolean := True) return Boolean
   is
      subtype Str8 is String (1 .. 8);

      Predef_Names : constant array (1 .. 11) of Str8 :=
        ("ada     ",       -- Ada
         "interfac",       -- Interfaces
         "system  ",       -- System

         --  Remaining entries are only considered if Renamings_Included true

         "calendar",       -- Calendar
         "machcode",       -- Machine_Code
         "unchconv",       -- Unchecked_Conversion
         "unchdeal",       -- Unchecked_Deallocation
         "directio",       -- Direct_IO
         "ioexcept",       -- IO_Exceptions
         "sequenio",       -- Sequential_IO
         "text_io ");      -- Text_IO

         Num_Entries : constant Natural :=
                         3 + 8 * Boolean'Pos (Renamings_Included);

   begin
      --  Remove extension (if present)

      if Name_Len > 4 and then Name_Buffer (Name_Len - 3) = '.' then
         Name_Len := Name_Len - 4;
      end if;

      --  Definitely false if longer than 12 characters (8.3)

      if Name_Len > 8 then
         return False;

      --  Definitely predefined if prefix is a- i- or s- followed by letter

      elsif Name_Len >=  3
        and then Name_Buffer (2) = '-'
        and then (Name_Buffer (1) = 'a'
                    or else
                  Name_Buffer (1) = 'i'
                    or else
                  Name_Buffer (1) = 's')
        and then (Name_Buffer (3) in 'a' .. 'z'
                    or else
                  Name_Buffer (3) in 'A' .. 'Z')
      then
         return True;
      end if;

      --  Otherwise check against special list, first padding to 8 characters

      while Name_Len < 8 loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ' ';
      end loop;

      for J in 1 .. Num_Entries loop
         if Name_Buffer (1 .. 8) = Predef_Names (J) then
            return True;
         end if;
      end loop;

      --  Note: when we return False here, the Name_Buffer contains the
      --  padded file name. This is not defined for clients of the package,
      --  but is used by Is_Internal_File_Name.

      return False;
   end Is_Predefined_File_Name;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      SFN_Table.Tree_Read;
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      SFN_Table.Tree_Write;
   end Tree_Write;

end Fname;
