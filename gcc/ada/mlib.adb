------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M L I B                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--           Copyright (C) 1999-2001, Ada Core Technologies, Inc.           --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Opt;
with Osint;    use Osint;
with Output;   use Output;
with MLib.Utl;

package body MLib is

   package Tools renames MLib.Utl;

   -------------------
   -- Build_Library --
   -------------------

   procedure Build_Library
     (Ofiles      : Argument_List;
      Afiles      : Argument_List;
      Output_File : String;
      Output_Dir  : String)
   is
      pragma Warnings (Off, Afiles);

      use GNAT.OS_Lib;

   begin
      if not Opt.Quiet_Output then
         Write_Line ("building a library...");
         Write_Str  ("   make ");
         Write_Line (Output_File);
      end if;

      Tools.Ar (Output_Dir & "/lib" & Output_File & ".a", Objects => Ofiles);

   end Build_Library;

   ------------------------
   -- Check_Library_Name --
   ------------------------

   procedure Check_Library_Name (Name : String) is
   begin
      if Name'Length = 0 then
         Fail ("library name cannot be empty");
      end if;

      if Name'Length > Max_Characters_In_Library_Name then
         Fail ("illegal library name """,
               Name,
               """: too long");
      end if;

      if not Is_Letter (Name (Name'First)) then
         Fail ("illegal library name """,
               Name,
               """: should start with a letter");
      end if;

      for Index in Name'Range loop
         if not Is_Alphanumeric (Name (Index)) then
            Fail ("illegal library name """,
                  Name,
                  """: should include only letters and digits");
         end if;
      end loop;
   end Check_Library_Name;

end MLib;
