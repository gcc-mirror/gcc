------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C A L E N D A R . T I M E _ Z O N E S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006, Free Software Foundation, Inc.            --
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
