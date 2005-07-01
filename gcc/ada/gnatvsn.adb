------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T V S N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2002-2003 Free Software Foundation, Inc.             --
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

package body Gnatvsn is

   Version_String : String (1 .. Ver_Len_Max);
   --  Import the C string defined in the (language-independent) source file
   --  version.c.
   --  The size is not the real one, which does not matter since we will
   --  check for the nul character in Gnat_Version_String.
   pragma Import (C, Version_String, "version_string");

   -------------------------
   -- Get_Gnat_Build_Type --
   -------------------------

   function Get_Gnat_Build_Type return Gnat_Build_Type is
   begin
      return FSF;
   end Get_Gnat_Build_Type;

   -------------------------
   -- Gnat_Version_String --
   -------------------------

   function Gnat_Version_String return String is
      NUL_Pos : Positive := 1;
   begin
      loop
         exit when Version_String (NUL_Pos) = ASCII.NUL;

         NUL_Pos := NUL_Pos + 1;
      end loop;

      return Version_String (1 .. NUL_Pos - 1);
   end Gnat_Version_String;

end Gnatvsn;
