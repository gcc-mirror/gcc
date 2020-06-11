------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                F N A M E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

package body Fname is

   function Has_Internal_Extension (Fname : String) return Boolean;
   pragma Inline (Has_Internal_Extension);
   --  True if the extension is appropriate for an internal/predefined unit.
   --  That means ".ads" or ".adb" for source files, and ".ali" for ALI files.

   function Has_Prefix (X, Prefix : String) return Boolean;
   pragma Inline (Has_Prefix);
   --  True if Prefix is at the beginning of X. For example,
   --  Has_Prefix ("a-filename.ads", Prefix => "a-") is True.

   ----------------------------
   -- Has_Internal_Extension --
   ----------------------------

   function Has_Internal_Extension (Fname : String) return Boolean is
   begin
      if Fname'Length >= 4 then
         declare
            S : String renames Fname (Fname'Last - 3 .. Fname'Last);
         begin
            return S = ".ads" or else S = ".adb" or else S = ".ali";
         end;
      end if;
      return False;
   end Has_Internal_Extension;

   ----------------
   -- Has_Prefix --
   ----------------

   function Has_Prefix (X, Prefix : String) return Boolean is
   begin
      if X'Length >= Prefix'Length then
         declare
            S : String renames X (X'First .. X'First + Prefix'Length - 1);
         begin
            return S = Prefix;
         end;
      end if;
      return False;
   end Has_Prefix;

   -----------------------
   -- Is_GNAT_File_Name --
   -----------------------

   function Is_GNAT_File_Name (Fname : String) return Boolean is
   begin
      --  Check for internal extensions before checking prefixes, so we don't
      --  think (e.g.) "gnat.adc" is internal.

      if not Has_Internal_Extension (Fname) then
         return False;
      end if;

      --  Definitely internal if prefix is g-

      if Has_Prefix (Fname, "g-") then
         return True;
      end if;

      --  See the note in Is_Predefined_File_Name for the rationale

      return Fname'Length = 8 and then Has_Prefix (Fname, "gnat");
   end Is_GNAT_File_Name;

   function Is_GNAT_File_Name (Fname : File_Name_Type) return Boolean is
      Result : constant Boolean :=
                 Is_GNAT_File_Name (Get_Name_String (Fname));
   begin
      return Result;
   end Is_GNAT_File_Name;

   ---------------------------
   -- Is_Internal_File_Name --
   ---------------------------

   function Is_Internal_File_Name
     (Fname              : String;
      Renamings_Included : Boolean := True) return Boolean
   is
   begin
      if Is_Predefined_File_Name (Fname, Renamings_Included) then
         return True;
      end if;

      return Is_GNAT_File_Name (Fname);
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
      --  Definitely false if longer than 12 characters (8.3)
      --  except for the Interfaces packages

      if Fname'Length > 12
        and then Fname (Fname'First .. Fname'First + 1) /= "i-"
      then
         return False;
      end if;

      if not Has_Internal_Extension (Fname) then
         return False;
      end if;

      --  Definitely predefined if prefix is a- i- or s-

      if Fname'Length >= 2 then
         declare
            S : String renames Fname (Fname'First .. Fname'First + 1);
         begin
            if S = "a-" or else S = "i-" or else S = "s-" then
               return True;
            end if;
         end;
      end if;

      --  We include the "." in the prefixes below, so we don't match (e.g.)
      --  adamant.ads. So the first line matches "ada.ads", "ada.adb", and
      --  "ada.ali". But that's not necessary if they have 8 characters.

      if Has_Prefix (Fname, "ada.")             --  Ada
        or else Has_Prefix (Fname, "interfac")  --  Interfaces
        or else Has_Prefix (Fname, "system.a")  --  System
      then
         return True;
      end if;

      --  If instructed and the name has 8+ characters, check for renamings

      if Renamings_Included
        and then Is_Predefined_Renaming_File_Name (Fname)
      then
         return True;
      end if;

      return False;
   end Is_Predefined_File_Name;

   function Is_Predefined_File_Name
     (Fname              : File_Name_Type;
      Renamings_Included : Boolean := True) return Boolean
   is
      Result : constant Boolean :=
                 Is_Predefined_File_Name
                   (Get_Name_String (Fname), Renamings_Included);
   begin
      return Result;
   end Is_Predefined_File_Name;

   --------------------------------------
   -- Is_Predefined_Renaming_File_Name --
   --------------------------------------

   function Is_Predefined_Renaming_File_Name
     (Fname : String) return Boolean
   is
      subtype Str8 is String (1 .. 8);

      Renaming_Names : constant array (1 .. 8) of Str8 :=
        ("calendar",   --  Calendar
         "machcode",   --  Machine_Code
         "unchconv",   --  Unchecked_Conversion
         "unchdeal",   --  Unchecked_Deallocation
         "directio",   --  Direct_IO
         "ioexcept",   --  IO_Exceptions
         "sequenio",   --  Sequential_IO
         "text_io.");  --  Text_IO
   begin
      --  Definitely false if longer than 12 characters (8.3)

      if Fname'Length in 8 .. 12 then
         declare
            S : String renames Fname (Fname'First .. Fname'First + 7);
         begin
            for J in Renaming_Names'Range loop
               if S = Renaming_Names (J) then
                  return True;
               end if;
            end loop;
         end;
      end if;

      return False;
   end Is_Predefined_Renaming_File_Name;

   function Is_Predefined_Renaming_File_Name
     (Fname : File_Name_Type) return Boolean is
      Result : constant Boolean :=
                 Is_Predefined_Renaming_File_Name (Get_Name_String (Fname));
   begin
      return Result;
   end Is_Predefined_Renaming_File_Name;

end Fname;
