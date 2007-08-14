------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   A D A . E X E C U T I O N _ T I M E                    --
--                                                                          --
--                                 S p e c                                  --
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
------------------------------------------------------------------------------

--  This is the Windows native version of this package

with Ada.Task_Identification;
with Ada.Real_Time;

package Ada.Execution_Time is

   type CPU_Time is private;

   CPU_Time_First : constant CPU_Time;
   CPU_Time_Last  : constant CPU_Time;
   CPU_Time_Unit  : constant := 0.000001;
   CPU_Tick       : constant Ada.Real_Time.Time_Span;

   function Clock
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task) return CPU_Time;

   function "+"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time;

   function "+"
     (Left  : Ada.Real_Time.Time_Span;
      Right : CPU_Time) return CPU_Time;

   function "-"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time;

   function "-"
     (Left  : CPU_Time;
      Right : CPU_Time) return Ada.Real_Time.Time_Span;

   function "<"  (Left, Right : CPU_Time) return Boolean;
   function "<=" (Left, Right : CPU_Time) return Boolean;
   function ">"  (Left, Right : CPU_Time) return Boolean;
   function ">=" (Left, Right : CPU_Time) return Boolean;

   procedure Split
     (T  : CPU_Time;
      SC : out Ada.Real_Time.Seconds_Count;
      TS : out Ada.Real_Time.Time_Span);

   function Time_Of
      (SC : Ada.Real_Time.Seconds_Count;
       TS : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero)
       return CPU_Time;

private

   type CPU_Time is new Ada.Real_Time.Time;

   CPU_Time_First : constant CPU_Time  := CPU_Time (Ada.Real_Time.Time_First);
   CPU_Time_Last  : constant CPU_Time  := CPU_Time (Ada.Real_Time.Time_Last);

   CPU_Tick : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Tick;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");

end Ada.Execution_Time;
