------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C A L E N D A R . T I M E _ Z O N E S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2009, Free Software Foundation, Inc.            --
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

package body Ada.Calendar.Time_Zones is

   --------------------------
   -- Implementation Notes --
   --------------------------

   --  All operations in this package are target and time representation
   --  independent, thus only one source file is needed for multiple targets.

   ---------------------
   -- UTC_Time_Offset --
   ---------------------

   function UTC_Time_Offset (Date : Time := Clock) return Time_Offset is
      Offset_L : constant Long_Integer :=
                   Time_Zones_Operations.UTC_Time_Offset (Date);
      Offset   : Time_Offset;

   begin
      if Offset_L = Invalid_Time_Zone_Offset then
         raise Unknown_Zone_Error;
      end if;

      --  The offset returned by Time_Zones_Operations.UTC_Time_Offset is in
      --  seconds, the returned value needs to be in minutes.

      Offset := Time_Offset (Offset_L / 60);

      --  Validity checks

      if not Offset'Valid then
         raise Unknown_Zone_Error;
      end if;

      return Offset;
   end UTC_Time_Offset;

end Ada.Calendar.Time_Zones;
