------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUN-TIME COMPONENTS                        --
--                                                                          --
--               S Y S T E M . V E R S I O N _ C O N T R O L                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

with System.Unsigned_Types; use System.Unsigned_Types;
package body System.Version_Control is

   ------------------------
   -- Get_Version_String --
   ------------------------

   function Get_Version_String
     (V    : System.Unsigned_Types.Unsigned)
      return Version_String
   is
      S : Version_String;
      D : Unsigned := V;
      H : constant array (Unsigned range 0 .. 15) of Character :=
            "0123456789abcdef";

   begin
      for J in reverse 1 .. 8 loop
         S (J) := H (D mod 16);
         D := D / 16;
      end loop;

      return S;
   end Get_Version_String;

end System.Version_Control;
