-- C3900011.AM
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that a record extension can be declared in the same package
--      as its parent, and that this parent may be a tagged record or a
--      record extension. Check that each derivative inherits all user-
--      defined primitive subprograms of its parent (including those that
--      its parent inherited), and that it may declare its own primitive
--      subprograms.
--
--      Check that predefined equality operators are defined for the root
--      tagged type.
--
--      Check that type conversion is defined from a type extension to its
--      parent, and that this parent itself may be a type extension.
--
-- TEST DESCRIPTION:
--      Declare a root tagged type in a package specification. Declare two
--      primitive subprograms for the type.
-- 
--      Extend the root type with a record extension in the same package
--      specification. Declare a new primitive subprogram for the extension
--      (in addition to its two inherited subprograms).
-- 
--      Extend the extension with a record extension in the same package
--      specification. Declare a new primitive subprogram for this second
--      extension (in addition to its three inherited subprograms).
-- 
--      In the main program, declare operations for the root tagged type which
--      utilize aggregates and equality operators to verify the correctness
--      of the components. Overload these operations for the two type
--      extensions. Within each of these overloading operations, utilize type
--      conversion to call the parent's implementation of the same operation.
-- 
-- TEST FILES:
--      The following files comprise this test:
--
--         C3900010.A
--      => C3900011.AM
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with C3900010;
with Report;
procedure C3900011 is


   package Check_Alert_Values is

      -- Declare functions to verify correctness of tagged record components
      -- before and after calls to their primitive subprograms.


      -- Alert_Type:

      function Initial_Values_Okay (A : in C3900010.Alert_Type)
        return Boolean;

      function Bad_Final_Values (A : in C3900010.Alert_Type)
        return Boolean;


      -- Low_Alert_Type:

      function Initial_Values_Okay (LA : in C3900010.Low_Alert_Type)
        return Boolean;

      function Bad_Final_Values (LA : in C3900010.Low_Alert_Type)
        return Boolean;


      -- Medium_Alert_Type:

      function Initial_Values_Okay (MA : in C3900010.Medium_Alert_Type)
        return Boolean;

      function Bad_Final_Values (MA : in C3900010.Medium_Alert_Type)
        return Boolean;


   end Check_Alert_Values;


        --==========================================================--


   package body Check_Alert_Values is


      function Initial_Values_Okay (A : in C3900010.Alert_Type)
        return Boolean is
         use type C3900010.Alert_Type;
      begin                                      -- "=" operator availability.
         return (A = (Arrival_Time => C3900010.Default_Time,
                      Display_On   => C3900010.Null_Device));
      end Initial_Values_Okay;


      function Initial_Values_Okay (LA : in C3900010.Low_Alert_Type)
        return Boolean is
      begin                                      -- Type conversion.
         return (Initial_Values_Okay (C3900010.Alert_Type (LA)) and
                 LA.Level = 0);                               
      end Initial_Values_Okay;


      function Initial_Values_Okay (MA : in C3900010.Medium_Alert_Type)
        return Boolean is
         use type C3900010.Person_Enum;
      begin                                      -- Type conversion.
         return (Initial_Values_Okay (C3900010.Low_Alert_Type (MA)) and
                 MA.Action_Officer = C3900010.Nobody);
      end Initial_Values_Okay;


      function Bad_Final_Values (A : in C3900010.Alert_Type)
        return Boolean is
         use type C3900010.Alert_Type;
      begin                                      -- "/=" operator availability.
         return (A /= (Arrival_Time => C3900010.Alert_Time,
                       Display_On   => C3900010.Null_Device));
      end Bad_Final_Values;


      function Bad_Final_Values (LA : in C3900010.Low_Alert_Type)
        return Boolean is
         use type C3900010.Low_Alert_Type;
      begin                                      -- "=" operator availability.
         return not ( LA = (Arrival_Time => C3900010.Alert_Time,
                            Display_On   => C3900010.Teletype,
                            Level        => 1) );
      end Bad_Final_Values;


      function Bad_Final_Values (MA : in C3900010.Medium_Alert_Type)
        return Boolean is
         use type C3900010.Medium_Alert_Type;
      begin                                      -- "/=" operator availability.
         return ( MA /= (C3900010.Alert_Time,
                         C3900010.Console,
                         1,
                         C3900010.Duty_Officer) );
      end Bad_Final_Values;


   end Check_Alert_Values;


        --==========================================================--


   use Check_Alert_Values;
   use C3900010;

   Root_Alarm   : C3900010.Alert_Type;
   Low_Alarm    : C3900010.Low_Alert_Type;
   Medium_Alarm : C3900010.Medium_Alert_Type;

begin

   Report.Test ("C390001", "Primitive operation inheritance by type " &
                "extensions: all extensions declared in same package " &
                "as parent");


-- Check root tagged type:

   if Initial_Values_Okay (Root_Alarm) then
      Handle  (Root_Alarm);                          -- Explicitly declared.
      Display (Root_Alarm);                          -- Explicitly declared.

      if Bad_Final_Values (Root_Alarm) then
         Report.Failed ("Wrong results after Alert_Type calls");
      end if;
   else
      Report.Failed ("Wrong initial values for Alert_Type");
   end if;


-- Check record extension of root tagged type:

   if Initial_Values_Okay (Low_Alarm) then
      Handle (Low_Alarm);                            -- Inherited.
      Low_Alarm.Display_On := Teletype;
      Display (Low_Alarm);                           -- Inherited.
      Low_Alarm.Level := Level_Of (Low_Alarm);       -- Explicitly declared.

      if Bad_Final_Values (Low_Alarm) then
         Report.Failed ("Wrong results after Low_Alert_Type calls");
      end if;
   else
      Report.Failed ("Wrong initial values for Low_Alert_Type");
   end if;


-- Check record extension of record extension:

   if Initial_Values_Okay (Medium_Alarm) then
      Handle (Medium_Alarm);                         -- Inherited twice.
      Medium_Alarm.Display_On := Console;
      Display (Medium_Alarm);                        -- Inherited twice.
      Medium_Alarm.Level := Level_Of (Medium_Alarm); -- Inherited.
      Assign_Officer (Medium_Alarm, Duty_Officer);   -- Explicitly declared.

      if Bad_Final_Values (Medium_Alarm) then
         Report.Failed ("Wrong results after Medium_Alert_Type calls");
      end if;
   else
      Report.Failed ("Wrong initial values for Medium_Alert_Type");
   end if;


-- Check final display counts:

   if C3900010.Display_Count_For /= (Null_Device => 1,
                                     Teletype    => 1,
                                     Console     => 1,
                                     Big_Screen  => 0)
   then
      Report.Failed ("Wrong final values for display counts");
   end if;


   Report.Result;

end C3900011;
