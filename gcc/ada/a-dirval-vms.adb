------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . D I R E C T O R I E S . V A L I D I T Y              --
--                                                                          --
--                                 B o d y                                  --
--                              (VMS Version)                               --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the OpenVMS version of this package

package body Ada.Directories.Validity is

   Max_Number_Of_Characters : constant := 39;
   Max_Path_Length          : constant := 1_024;

   Invalid_Character : constant array (Character) of Boolean :=
                         ('a' .. 'z' => False,
                          'A' .. 'Z' => False,
                          '_' | '$' | '-' | '.' => False,
                          others => True);

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
      First     : Positive := Name'First;
      Last      : Positive;
      Dot_Found : Boolean := False;

   begin
      --  A valid path (directory) name cannot be empty, and cannot contain
      --  more than 1024 characters. Directories can be ".", ".." or be simple
      --  name without extensions.

      if Name'Length = 0 or else Name'Length > Max_Path_Length then
         return False;

      else
         loop
            --  Look for the start of the next directory or file name

            while First <= Name'Last and then Name (First) = '/' loop
               First := First + 1;
            end loop;

            --  If all directories/file names are OK, return True

            exit when First > Name'Last;

            Last := First;
            Dot_Found := False;

            --  Look for the end of the directory/file name

            while Last < Name'Last loop
               exit when Name (Last + 1) = '/';
               Last := Last + 1;

               if Name (Last) = '.' then
                  Dot_Found := True;
               end if;
            end loop;

            --  If name include a dot, it can only be ".", ".." or a the last
            --  file name.

            if Dot_Found then
               if Name (First .. Last) /= "." and then
                  Name (First .. Last) /= ".."
               then
                  return Last = Name'Last
                    and then Is_Valid_Simple_Name (Name (First .. Last));

               end if;

            --  Check if the directory/file name is valid

            elsif not Is_Valid_Simple_Name (Name (First .. Last)) then
                  return False;
            end if;

            --  Move to the next name

            First := Last + 1;
         end loop;
      end if;

      --  If Name follows the rules, then it is valid

      return True;
   end Is_Valid_Path_Name;

   --------------------------
   -- Is_Valid_Simple_Name --
   --------------------------

   function Is_Valid_Simple_Name (Name : String) return Boolean is
      In_Extension         : Boolean := False;
      Number_Of_Characters : Natural := 0;

   begin
      --  A file name cannot be empty, and cannot have more than 39 characters
      --  before or after a single '.'.

      if Name'Length = 0 then
         return False;

      else
         --  Check each character for validity

         for J in Name'Range loop
            if Invalid_Character (Name (J)) then
               return False;

            elsif Name (J) = '.' then

               --  Name cannot contain several dots

               if In_Extension then
                  return False;

               else
                  --  Reset the number of characters to count the characters
                  --  of the extension.

                  In_Extension := True;
                  Number_Of_Characters := 0;
               end if;

            else
               --  Check that the number of character is not too large

               Number_Of_Characters := Number_Of_Characters + 1;

               if Number_Of_Characters > Max_Number_Of_Characters then
                  return False;
               end if;
            end if;
         end loop;
      end if;

      --  If the rules are followed, then it is valid

      return True;
   end Is_Valid_Simple_Name;

   -------------
   -- OpenVMS --
   -------------

   function OpenVMS return Boolean is
   begin
      return True;
   end OpenVMS;

end Ada.Directories.Validity;
