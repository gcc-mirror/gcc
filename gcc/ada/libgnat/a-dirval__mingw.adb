------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . D I R E C T O R I E S . V A L I D I T Y              --
--                                                                          --
--                                 B o d y                                  --
--                            (Windows Version)                             --
--                                                                          --
--          Copyright (C) 2004-2024, Free Software Foundation, Inc.         --
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

--  This is the Windows version of this package

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body Ada.Directories.Validity is

   Invalid_Character : constant array (Character) of Boolean :=
     (NUL .. US | '\'       => True,
      '/' | ':' | '*' | '?' => True,
      '"' | '<' | '>' | '|' => True,
      DEL                   => True,
      others                => False);
   --  Note that a valid file-name or path-name is implementation defined.
   --  To support UTF-8 file and directory names, we do not want to be too
   --  restrictive here.

   ---------------------------------
   -- Is_Path_Name_Case_Sensitive --
   ---------------------------------

   function Is_Path_Name_Case_Sensitive return Boolean is
   begin
      return False;
   end Is_Path_Name_Case_Sensitive;

   ------------------------
   -- Is_Valid_Path_Name --
   ------------------------

   function Is_Valid_Path_Name (Name : String) return Boolean is
      Start : Positive := Name'First;
      Last  : Natural;

   begin
      --  A path name cannot be empty, cannot contain more than 256 characters,
      --  cannot contain invalid characters and each directory/file name need
      --  to be valid.

      if Name'Length = 0 or else Name'Length > 256 then
         return False;

      else
         --  A drive letter may be specified at the beginning

         if Name'Length >= 2
           and then Name (Start + 1) = ':'
           and then
             (Name (Start) in 'A' .. 'Z' or else Name (Start) in 'a' .. 'z')
         then
            Start := Start + 2;

            --  A drive letter followed by a colon and followed by nothing or
            --  by a relative path is an ambiguous path name on Windows, so we
            --  don't accept it.

            if Start > Name'Last
              or else (Name (Start) /= '/' and then Name (Start) /= '\')
            then
               return False;
            end if;
         end if;

         loop
            --  Look for the start of the next directory or file name

            while Start <= Name'Last
              and then (Name (Start) = '\' or Name (Start) = '/')
            loop
               Start := Start + 1;
            end loop;

            --  If all directories/file names are OK, return True

            exit when Start > Name'Last;

            Last := Start;

            --  Look for the end of the directory/file name

            while Last < Name'Last loop
               exit when Name (Last + 1) = '\' or Name (Last + 1) = '/';
               Last := Last + 1;
            end loop;

            --  Check if the directory/file name is valid

            if not Is_Valid_Simple_Name (Name (Start .. Last)) then
                  return False;
            end if;

            --  Move to the next name

            Start := Last + 1;
         end loop;
      end if;

      --  If Name follows the rules, it is valid

      return True;
   end Is_Valid_Path_Name;

   --------------------------
   -- Is_Valid_Simple_Name --
   --------------------------

   function Is_Valid_Simple_Name (Name : String) return Boolean is
      Only_Spaces : Boolean;

   begin
      --  A file name cannot be empty, cannot contain more than 256 characters,
      --  and cannot contain invalid characters.

      if Name'Length = 0 or else Name'Length > 256 then
         return False;

      --  Name length is OK

      else
         Only_Spaces := True;
         for J in Name'Range loop
            if Invalid_Character (Name (J)) then
               return False;
            elsif Name (J) /= ' ' then
               Only_Spaces := False;
            end if;
         end loop;

         --  If no invalid chars, and not all spaces, file name is valid

         return not Only_Spaces;
      end if;
   end Is_Valid_Simple_Name;

   -------------
   -- Windows --
   -------------

   function Windows return Boolean is
   begin
      return True;
   end Windows;

end Ada.Directories.Validity;
