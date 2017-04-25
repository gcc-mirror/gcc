------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                F N A M E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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
with Table;
with Types; use Types;

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

   function Has_Internal_Extension (Fname : String) return Boolean;
   --  True if the extension is appropriate for an internal/predefined
   --  unit. That means ".ads" or ".adb" for source files, and ".ali" for
   --  ALI files.

   function Has_Prefix (X, Prefix : String) return Boolean;
   --  True if Prefix is at the beginning of X. For example,
   --  Has_Prefix("a-filename.ads", Prefix => "a-") is True.

   function Has_Suffix (X, Suffix : String) return Boolean;
   --  True if Suffix is at the end of X

   ----------------------------
   -- Has_Internal_Extension --
   ----------------------------

   function Has_Internal_Extension (Fname : String) return Boolean is
   begin
      return
        Has_Suffix (Fname, Suffix => ".ads")
          or else Has_Suffix (Fname, Suffix => ".adb")
          or else Has_Suffix (Fname, Suffix => ".ali");
   end Has_Internal_Extension;

   ----------------
   -- Has_Prefix --
   ----------------

   function Has_Prefix (X, Prefix : String) return Boolean is
   begin
      if X'Length >= Prefix'Length then
         declare
            Slice : String renames
                      X (X'First .. X'First + Prefix'Length - 1);
         begin
            return Slice = Prefix;
         end;
      end if;
      return False;
   end Has_Prefix;

   ----------------
   -- Has_Suffix --
   ----------------

   function Has_Suffix (X, Suffix : String) return Boolean is
   begin
      if X'Length >= Suffix'Length then
         declare
            Slice : String renames
                      X (X'Last - Suffix'Length + 1 .. X'Last);
         begin
            return Slice = Suffix;
         end;
      end if;
      return False;
   end Has_Suffix;

   ---------------------------
   -- Is_Internal_File_Name --
   ---------------------------

   function Is_Internal_File_Name
     (Fname              : String;
      Renamings_Included : Boolean := True) return Boolean
   is
   begin
      --  Check for internal extensions first, so we don't think (e.g.)
      --  "gnat.adc" is internal.

      if not Has_Internal_Extension (Fname) then
         return False;
      end if;

      return
        Is_Predefined_File_Name (Fname, Renamings_Included)
          or else Has_Prefix (Fname, Prefix => "g-")
          or else Has_Prefix (Fname, Prefix => "gnat.ad");
   end Is_Internal_File_Name;

   function Is_Internal_File_Name
     (Fname              : File_Name_Type;
      Renamings_Included : Boolean := True) return Boolean
   is
      Result : constant Boolean :=
                 Is_Internal_File_Name
                   (Get_Name_String (Fname), Renamings_Included);
   begin
      return Result;
   end Is_Internal_File_Name;

   -----------------------------
   -- Is_Predefined_File_Name --
   -----------------------------

   function Is_Predefined_File_Name
     (Fname              : String;
      Renamings_Included : Boolean := True) return Boolean
   is
   begin
      if not Has_Internal_Extension (Fname) then
         return False;
      end if;

      if Has_Prefix (Fname, "a-")
        or else Has_Prefix (Fname, "i-")
        or else Has_Prefix (Fname, "s-")
      then
         return True;
      end if;

      --  Definitely false if longer than 12 characters (8.3)

      if Fname'Length > 12 then
         return False;
      end if;

      if Has_Prefix (Fname, Prefix => "ada.ad")              --  Ada
        or else Has_Prefix (Fname, Prefix => "interfac.ad")  --  Interfaces
        or else Has_Prefix (Fname, Prefix => "system.ad")    --  System
      then
         return True;
      end if;

      if not Renamings_Included then
         return False;
      end if;

      --  The following are the predefined renamings

      return
         --  Calendar

        Has_Prefix (Fname, Prefix => "calendar.ad")

         --  Machine_Code

          or else Has_Prefix (Fname, Prefix => "machcode.ad")

         --  Unchecked_Conversion

          or else Has_Prefix (Fname, Prefix => "unchconv.ad")

         --  Unchecked_Deallocation

          or else Has_Prefix (Fname, Prefix => "unchdeal.ad")

         --  Direct_IO

          or else Has_Prefix (Fname, Prefix => "directio.ad")

         --  IO_Exceptions

          or else Has_Prefix (Fname, Prefix => "ioexcept.ad")

         --  Sequential_IO

          or else Has_Prefix (Fname, Prefix => "sequenio.ad")

         --  Text_IO

          or else Has_Prefix (Fname, Prefix => "text_io.ad");
   end Is_Predefined_File_Name;

   function Is_Predefined_File_Name
     (Fname              : File_Name_Type;
      Renamings_Included : Boolean := True) return Boolean
   is
   begin
      return
        Is_Predefined_File_Name
          (Get_Name_String (Fname), Renamings_Included);
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
