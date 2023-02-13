------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                G N A T . C A L E N D A R . T I M E _ I O                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2023, AdaCore                     --
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

--  This package augments standard Ada.Text_IO with facilities for input
--  and output of time values in standardized format.

with Ada.Calendar.Time_Zones; use Ada.Calendar;

package GNAT.Calendar.Time_IO is

   Picture_Error : exception;
   --  Exception raised for incorrect picture

   type Picture_String is new String;
   --  This is a string to describe date and time output format. The string is
   --  a set of standard character and special tag that are replaced by the
   --  corresponding values. It follows the GNU Date specification. Here are
   --  the recognized directives:
   --
   --          %    a literal %
   --          n    a newline
   --          t    a horizontal tab
   --
   --          Time fields:
   --
   --          %H   hour (00..23)
   --          %I   hour (01..12)
   --          %k   hour ( 0..23)
   --          %l   hour ( 1..12)
   --          %M   minute (00..59)
   --          %p   locale's AM or PM
   --          %r   time, 12-hour (hh:mm:ss [AP]M)
   --          %s   seconds  since 1970-01-01  00:00:00 UTC
   --                (a nonstandard extension)
   --          %S   second (00..59)
   --          %T   time, 24-hour (hh:mm:ss)
   --          %:::z  numeric time zone with : to necessary precision
   --                 (e.g., -04, +05:30)
   --
   --          Date fields:
   --
   --          %a   locale's abbreviated weekday name (Sun..Sat)
   --          %A   locale's    full   weekday   name,    variable   length
   --                  (Sunday..Saturday)
   --          %b   locale's abbreviated month name (Jan..Dec)
   --          %B   locale's    full    month    name,   variable    length
   --                  (January..December)
   --          %c   locale's date and time (Sat Nov 04 12:02:33 EST 1989)
   --          %d   day of month (01..31)
   --          %D   date (mm/dd/yy)
   --          %h   same as %b
   --          %j   day of year (001..366)
   --          %m   month (01..12)
   --          %U   week number  of year with  Sunday as first day  of week
   --                  (00..53)
   --          %w   day of week (0..6) with 0 corresponding to Sunday
   --          %W   week number  of year with  Monday as first day  of week
   --                  (00..53)
   --          %x   locale's date representation (mm/dd/yy)
   --          %y   last two digits of year (00..99)
   --          %Y   year (1970...)
   --
   --          By default,  date pads numeric fields with zeroes.  GNU date
   --          recognizes the following nonstandard numeric modifiers:
   --
   --          -    (hyphen) do not pad the field
   --          _    (underscore) pad the field with spaces
   --
   --  Here are some GNAT extensions to the GNU Date specification:
   --
   --          %i   milliseconds (3 digits)
   --          %e   microseconds (6 digits)
   --          %o   nanoseconds  (9 digits)

   ISO_Time : constant Picture_String;
   --  ISO 8601 standard date and time, with time zone.

   ISO_Date : constant Picture_String;
   --  This format follows the ISO 8601 standard. The format is "YYYY-MM-DD",
   --  four digits year, month and day number separated by minus.

   US_Date : constant Picture_String;
   --  This format is the common US date format: "MM/DD/YY",
   --  month and day number, two digits year separated by slashes.

   European_Date : constant Picture_String;
   --  This format is the common European date format: "DD/MM/YY",
   --  day and month number, two digits year separated by slashes.

   function Image
     (Date    : Ada.Calendar.Time;
      Picture : Picture_String) return String;
   --  Return Date, as interpreted in the current local time zone, as a string
   --  with format Picture. Raise Picture_Error if picture string is null or
   --  has an incorrect format.

   function Image
     (Date      : Ada.Calendar.Time;
      Picture   : Picture_String;
      Time_Zone : Time_Zones.Time_Offset) return String;
   --  Same as previous Image, except it uses the specified time zone instead
   --  of the local time zone.

   function Value (Date : String) return Ada.Calendar.Time;
   --  Parse the string Date, interpreted as a time representation in the
   --  current local time zone, and return the corresponding Time value. The
   --  following time format is supported:
   --
   --     hh:mm:ss             - Date is the current date
   --
   --  The following formats are also supported. They all accept an optional
   --  time with the format "hh:mm:ss". The time is separated from the date by
   --  exactly one space character.
   --
   --  When the time is not specified, it is set to 00:00:00. The delimiter '*'
   --  must be either '-' and '/' and both occurrences must use the same
   --  character.
   --
   --  Trailing characters (in particular spaces) are not allowed
   --
   --     yyyy*mm*dd           - ISO format
   --     yy*mm*dd             - Year is assumed to be 20yy
   --     mm*dd*yyyy           - (US date format)
   --     dd*mmm*yyyy          - month spelled out
   --     yyyy*mmm*dd          - month spelled out
   --     yyyymmdd             - Iso format, no separator
   --     mmm dd, yyyy         - month spelled out
   --     dd mmm yyyy          - month spelled out
   --
   --  The following ISO-8601 format expressed as a regular expression is also
   --  supported:
   --
   --    (yyyymmdd | yyyy'-'mm'-'dd)'T'(hhmmss | hh':'mm':'ss)
   --      [ ('.' | ',') s{s} ]
   --      [ ('Z' | ('+'|'-')hh':'mm) ]
   --  Trailing characters (including spaces) are not allowed.
   --  In the ISO case, the current time zone is not used; the time zone
   --  is as specified in the string, defaulting to UTC.
   --
   --  Examples:
   --
   --    2017-04-14T14:47:06      20170414T14:47:06       20170414T144706
   --    2017-04-14T14:47:06,1234 20170414T14:47:06.1234
   --    2017-04-14T19:47:06+05   20170414T09:00:06-05:47

   --  Constraint_Error is raised if the input string is malformed (does not
   --  conform to one of the above dates, or has an invalid time string), or
   --  the resulting time is not valid.

   procedure Put_Time (Date : Ada.Calendar.Time; Picture : Picture_String);
   --  Put Date with format Picture. Raise Picture_Error if bad picture string

private
   ISO_Time      : constant Picture_String := "%Y-%m-%dT%H:%M:%S%:::z";
   ISO_Date      : constant Picture_String := "%Y-%m-%d";
   US_Date       : constant Picture_String := "%m/%d/%y";
   European_Date : constant Picture_String := "%d/%m/%y";

end GNAT.Calendar.Time_IO;
