------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C A L E N D A R . T I M E _ Z O N E S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides routines to determine the offset of dates to GMT.
--  It is defined in the Ada 2005 RM (9.6.1).

package Ada.Calendar.Time_Zones is

   --  Time zone manipulation

   type Time_Offset is range -(28 * 60) .. 28 * 60;

   Unknown_Zone_Error : exception;

   function UTC_Time_Offset return Time_Offset;
   --  Returns (in minutes), the difference between the implementation-defined
   --  time zone of Calendar, and UTC time. If the time zone of the Calendar
   --  implementation is unknown, raises Unknown_Zone_Error.

   function UTC_Time_Offset (Date : Time) return Time_Offset;
   --  Returns (in minutes), the difference between the implementation-defined
   --  time zone of Calendar, and UTC time, at the time Date. If the time zone
   --  of the Calendar implementation is unknown, raises Unknown_Zone_Error.

end Ada.Calendar.Time_Zones;
