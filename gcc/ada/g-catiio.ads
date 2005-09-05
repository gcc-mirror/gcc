------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                G N A T . C A L E N D A R . T I M E _ I O                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2005, AdaCore                     --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This package augments standard Ada.Text_IO with facilities for input
--  and output of time values in standardized format.

package GNAT.Calendar.Time_IO is

   Picture_Error : exception;
   --  Exception raised for incorrect picture

   type Picture_String is new String;
   --  This is a string to describe date and time output format. The string is
   --  a set of standard character and special tag that are replaced by the
   --  corresponding values. It follows the GNU Date specification. Here are
   --  the recognized directives :
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

   ISO_Date : constant Picture_String;
   --  This format follow the ISO 8601 standard. The format is "YYYY-MM-DD",
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
   --  Return Date as a string with format Picture. Raise Picture_Error if
   --  picture string is wrong.

   procedure Put_Time
     (Date    : Ada.Calendar.Time;
      Picture : Picture_String);
   --  Put Date with format Picture. Raise Picture_Error if picture string is
   --  wrong

private
   ISO_Date      : constant Picture_String := "%Y-%m-%d";
   US_Date       : constant Picture_String := "%m/%d/%y";
   European_Date : constant Picture_String := "%d/%m/%y";

end GNAT.Calendar.Time_IO;
