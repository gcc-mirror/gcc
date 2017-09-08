------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C A L E N D A R . F O R M A T T I N G               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides additional components to Time, as well as new
--  Time_Of and Split routines which handle time zones and leap seconds.
--  This package is defined in the Ada 2005 RM (9.6.1).

with Ada.Calendar.Time_Zones;

package Ada.Calendar.Formatting is

   --  Day of the week

   type Day_Name is
     (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

   function Day_Of_Week (Date : Time) return Day_Name;

   --  Hours:Minutes:Seconds access

   subtype Hour_Number     is Natural range 0 .. 23;
   subtype Minute_Number   is Natural range 0 .. 59;
   subtype Second_Number   is Natural range 0 .. 59;
   subtype Second_Duration is Day_Duration range 0.0 .. 1.0;

   function Year
     (Date      : Time;
      Time_Zone : Time_Zones.Time_Offset := 0) return Year_Number;

   function Month
     (Date      : Time;
      Time_Zone : Time_Zones.Time_Offset := 0) return Month_Number;

   function Day
     (Date      : Time;
      Time_Zone : Time_Zones.Time_Offset := 0) return Day_Number;

   function Hour
     (Date      : Time;
      Time_Zone : Time_Zones.Time_Offset := 0) return Hour_Number;

   function Minute
     (Date      : Time;
      Time_Zone : Time_Zones.Time_Offset := 0) return Minute_Number;

   function Second
     (Date : Time)                             return Second_Number;

   function Sub_Second
     (Date : Time)                             return Second_Duration;

   function Seconds_Of
     (Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number := 0;
      Sub_Second : Second_Duration := 0.0) return Day_Duration;
   --  Returns a Day_Duration value for the combination of the given Hour,
   --  Minute, Second, and Sub_Second. This value can be used in Ada.Calendar.
   --  Time_Of as well as the argument to Calendar."+" and Calendar."-". If
   --  Seconds_Of is called with a Sub_Second value of 1.0, the value returned
   --  is equal to the value of Seconds_Of for the next second with a Sub_
   --  Second value of 0.0.

   procedure Split
     (Seconds    : Day_Duration;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration);
   --  Splits Seconds into Hour, Minute, Second and Sub_Second in such a way
   --  that the resulting values all belong to their respective subtypes. The
   --  value returned in the Sub_Second parameter is always less than 1.0.

   procedure Split
     (Date       : Time;
      Year       : out Year_Number;
      Month      : out Month_Number;
      Day        : out Day_Number;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration;
      Time_Zone  : Time_Zones.Time_Offset := 0);
   --  Splits Date into its constituent parts (Year, Month, Day, Hour, Minute,
   --  Second, Sub_Second), relative to the specified time zone offset. The
   --  value returned in the Sub_Second parameter is always less than 1.0.

   function Time_Of
     (Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration := 0.0;
      Leap_Second : Boolean := False;
      Time_Zone   : Time_Zones.Time_Offset := 0) return Time;
   --  If Leap_Second is False, returns a Time built from the date and time
   --  values, relative to the specified time zone offset. If Leap_Second is
   --  True, returns the Time that represents the time within the leap second
   --  that is one second later than the time specified by the parameters.
   --  Time_Error is raised if the parameters do not form a proper date or
   --  time. If Time_Of is called with a Sub_Second value of 1.0, the value
   --  returned is equal to the value of Time_Of for the next second with a
   --  Sub_Second value of 0.0.

   function Time_Of
     (Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Seconds     : Day_Duration := 0.0;
      Leap_Second : Boolean := False;
      Time_Zone   : Time_Zones.Time_Offset := 0) return Time;
   --  If Leap_Second is False, returns a Time built from the date and time
   --  values, relative to the specified time zone offset. If Leap_Second is
   --  True, returns the Time that represents the time within the leap second
   --  that is one second later than the time specified by the parameters.
   --  Time_Error is raised if the parameters do not form a proper date or
   --  time. If Time_Of is called with a Seconds value of 86_400.0, the value
   --  returned is equal to the value of Time_Of for the next day with a
   --  Seconds value of 0.0.

   procedure Split
     (Date        : Time;
      Year        : out Year_Number;
      Month       : out Month_Number;
      Day         : out Day_Number;
      Hour        : out Hour_Number;
      Minute      : out Minute_Number;
      Second      : out Second_Number;
      Sub_Second  : out Second_Duration;
      Leap_Second : out Boolean;
      Time_Zone   : Time_Zones.Time_Offset := 0);
   --  If Date does not represent a time within a leap second, splits Date
   --  into its constituent parts (Year, Month, Day, Hour, Minute, Second,
   --  Sub_Second), relative to the specified time zone offset, and sets
   --  Leap_Second to False. If Date represents a time within a leap second,
   --  set the constituent parts to values corresponding to a time one second
   --  earlier than that given by Date, relative to the specified time zone
   --  offset, and sets Leap_Seconds to True. The value returned in the
   --  Sub_Second parameter is always less than 1.0.

   procedure Split
     (Date        : Time;
      Year        : out Year_Number;
      Month       : out Month_Number;
      Day         : out Day_Number;
      Seconds     : out Day_Duration;
      Leap_Second : out Boolean;
      Time_Zone   : Time_Zones.Time_Offset := 0);
   --  If Date does not represent a time within a leap second, splits Date
   --  into its constituent parts (Year, Month, Day, Seconds), relative to the
   --  specified time zone offset, and sets Leap_Second to False. If Date
   --  represents a time within a leap second, set the constituent parts to
   --  values corresponding to a time one second earlier than that given by
   --  Date, relative to the specified time zone offset, and sets Leap_Seconds
   --  to True. The value returned in the Seconds parameter is always less
   --  than 86_400.0.

   --  Simple image and value

   function Image
     (Date                  : Time;
      Include_Time_Fraction : Boolean := False;
      Time_Zone             : Time_Zones.Time_Offset := 0) return String;
   --  Returns a string form of the Date relative to the given Time_Zone. The
   --  format is "Year-Month-Day Hour:Minute:Second", where the Year is a
   --  4-digit value, and all others are 2-digit values, of the functions
   --  defined in Ada.Calendar and Ada.Calendar.Formatting, including a
   --  leading zero, if needed. The separators between the values are a minus,
   --  another minus, a colon, and a single space between the Day and Hour. If
   --  Include_Time_Fraction is True, the integer part of Sub_Seconds*100 is
   --  suffixed to the string as a point followed by a 2-digit value.

   function Value
     (Date       : String;
      Time_Zone  : Time_Zones.Time_Offset := 0) return Time;
   --  Returns a Time value for the image given as Date, relative to the given
   --  time zone. Constraint_Error is raised if the string is not formatted as
   --  described for Image, or the function cannot interpret the given string
   --  as a Time value.

   function Image
     (Elapsed_Time          : Duration;
      Include_Time_Fraction : Boolean := False) return String;
   --  Returns a string form of the Elapsed_Time. The format is "Hour:Minute:
   --  Second", where all values are 2-digit values, including a leading zero,
   --  if needed. The separators between the values are colons. If Include_
   --  Time_Fraction is True, the integer part of Sub_Seconds*100 is suffixed
   --  to the string as a point followed by a 2-digit value. If Elapsed_Time <
   --  0.0, the result is Image (abs Elapsed_Time, Include_Time_Fraction)
   --  prefixed with a minus sign. If abs Elapsed_Time represents 100 hours or
   --  more, the result is implementation-defined.

   function Value (Elapsed_Time : String) return Duration;
   --  Returns a Duration value for the image given as Elapsed_Time.
   --  Constraint_Error is raised if the string is not formatted as described
   --  for Image, or the function cannot interpret the given string as a
   --  Duration value.

end Ada.Calendar.Formatting;
