------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       G N A T . T I M E _ S T A M P                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2008, Free Software Foundation, Inc.           --
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

with Interfaces.C; use Interfaces.C;

package body GNAT.Time_Stamp is

   subtype time_stamp is char_array (0 .. 22);
   type time_stamp_ptr is access all time_stamp;
   --  The desired ISO 8601 string format has exactly 22 characters. We add
   --  one additional character for '\0'. The indexing starts from zero to
   --  accommodate the C layout.

   procedure gnat_current_time_string (Value : time_stamp_ptr);
   pragma Import (C, gnat_current_time_string, "__gnat_current_time_string");

   ------------------
   -- Current_Time --
   ------------------

   function Current_Time return String is
      Result : aliased time_stamp;

   begin
      gnat_current_time_string (Result'Unchecked_Access);
      Result (22) := nul;

      return To_Ada (Result);
   end Current_Time;

end GNAT.Time_Stamp;
