------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T V S N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

package body Gnatvsn is

   ----------------------
   -- Copyright_Holder --
   ----------------------

   function Copyright_Holder return String is
   begin
      return "Free Software Foundation, Inc.";
   end Copyright_Holder;

   ------------------------
   -- Gnat_Free_Software --
   ------------------------

   function Gnat_Free_Software return String is
   begin
      return
        "This is free software; see the source for copying conditions." &
        ASCII.LF &
        "There is NO warranty; not even for MERCHANTABILITY or FITNESS" &
        " FOR A PARTICULAR PURPOSE.";
   end Gnat_Free_Software;

   type char_array is array (Natural range <>) of aliased Character;
   Version_String : char_array (0 .. Ver_Len_Max - 1);
   --  Import the C string defined in the (language-independent) source file
   --  version.c using the zero-based convention of the C language.
   --  The size is not the real one, which does not matter since we will
   --  check for the nul character in Gnat_Version_String.
   pragma Import (C, Version_String, "gnat_version_string");

   -------------------------
   -- Gnat_Version_String --
   -------------------------

   function Gnat_Version_String return String is
      S : String (1 .. Ver_Len_Max);
      Pos : Natural := 0;
   begin
      loop
         exit when Version_String (Pos) = ASCII.NUL;

         S (Pos + 1) := Version_String (Pos);
         Pos := Pos + 1;

         exit when Pos = Ver_Len_Max;
      end loop;

      return S (1 .. Pos);
   end Gnat_Version_String;

end Gnatvsn;
