------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       G N A T . T I M E _ S T A M P                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2008-2025, Free Software Foundation, Inc.        --
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
