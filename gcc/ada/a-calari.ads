------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C A L E N D A R . A R I T H M E T I C               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides arithmetic operations of time values using days
--  and leap seconds. Ada.Calendar.Arithmetic is defined in the Ada 2005
--  RM (9.6.1).

package Ada.Calendar.Arithmetic is

   --  Arithmetic on days:

   --  Rough estimate on the number of days over the range of Ada time

   type Day_Count is range
     -(366 * (1 + Year_Number'Last - Year_Number'First))
        ..
     +(366 * (1 + Year_Number'Last - Year_Number'First));

   subtype Leap_Seconds_Count is Integer range -2047 .. 2047;
   --  Count of leap seconds. Negative leap seconds occur whenever the
   --  astronomical time is faster than the atomic time or as a result of
   --  Difference when Left < Right.

   procedure Difference
     (Left         : Time;
      Right        : Time;
      Days         : out Day_Count;
      Seconds      : out Duration;
      Leap_Seconds : out Leap_Seconds_Count);
   --  Returns the difference between Left and Right. Days is the number of
   --  days of difference, Seconds is the remainder seconds of difference
   --  excluding leap seconds, and Leap_Seconds is the number of leap seconds.
   --  If Left < Right, then Seconds <= 0.0, Days <= 0, and Leap_Seconds <= 0,
   --  otherwise all values are nonnegative. The absolute value of Seconds is
   --  always less than 86_400.0. For the returned values, if Days = 0, then
   --  Seconds + Duration (Leap_Seconds) = Calendar."-" (Left, Right)

   function "+" (Left : Time;      Right : Day_Count) return Time;
   function "+" (Left : Day_Count; Right : Time)      return Time;
   --  Adds a number of days to a time value. Time_Error is raised if the
   --  result is not representable as a value of type Time.

   function "-" (Left : Time;      Right : Day_Count) return Time;
   --  Subtracts a number of days from a time value. Time_Error is raised if
   --  the result is not representable as a value of type Time.

   function "-" (Left : Time;      Right : Time)      return Day_Count;
   --  Subtracts two time values, and returns the number of days between them.
   --  This is the same value that Difference would return in Days.

end Ada.Calendar.Arithmetic;
