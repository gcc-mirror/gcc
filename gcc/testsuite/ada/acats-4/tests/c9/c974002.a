-- C974002.A
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
--      Check that the sequence of statements of the triggering alternative
--      of an asynchronous select statement is executed if the triggering
--      statement is a delay_until statement, and the specified time has
--      already passed. Check that the abortable part is not executed after
--      the sequence of statements of the triggering alternative is left.
--
--      Check that the sequence of statements of the triggering alternative
--      of an asynchronous select statement is not executed if the abortable
--      part completes before the triggering statement, and the triggering
--      statement is a delay_until statement.
--
-- TEST DESCRIPTION:
--      Declare a task with an accept statement containing an asynchronous
--      select with a delay_until triggering statement. Parameterize
--      the accept statement with the time to be used in the delay. Simulate
--      a quick calculation by declaring a procedure which sets a Boolean
--      flag. Call this procedure in the abortable part.
--
--      Make two calls to the task entry: (1) with a time that has already
--      expired, and (2) with a time that will not expire before the quick
--      calculation completes.
--
--      For (1), the sequence of statements following the triggering statement
--      is executed, and the abortable part never starts.
--
--      For (2), the abortable part completes before the triggering statement,
--      the delay is canceled, and the sequence of statements following the
--      triggering statement never starts.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      26 Nov 95   SAIC    Bug fix for ACVC 2.0.1.
--
--!

with Report;
with Ada.Calendar;
with ImpDef;
procedure C974002 is

   function "-" (Left: Ada.Calendar.Time; Right: Duration )
                          return Ada.Calendar.Time renames Ada.Calendar."-";
   function "+" (Left: Ada.Calendar.Time; Right: Duration ) 
                          return Ada.Calendar.Time renames Ada.Calendar."+";
   
   Abortable_Part_Executed         : Boolean;
   Triggering_Alternative_Executed : Boolean;


          --========================================================--


   procedure Quick_Calculation is
   begin
      if Report.Equal (1, 1) then
         Abortable_Part_Executed := True;
      end if;
   end Quick_Calculation;


          --========================================================--


   task type Timed_Calculation_Task is
      entry Calculation (Time_Out : in Ada.Calendar.Time);
   end Timed_Calculation_Task;


   task body Timed_Calculation_Task is
   begin
      loop
         select
            accept Calculation (Time_Out : in Ada.Calendar.Time) do

               --                                    --
               -- Asynchronous select is tested here --
               --                                    --

               select
                  delay until Time_Out;                    -- Triggering 
                                                           -- statement.

                  Triggering_Alternative_Executed := True; -- Triggering
                                                           -- alternative.
               then abort
                  Quick_Calculation;                       -- Abortable part.
               end select;
            end Calculation;
         or
            terminate;
         end select;
      end loop;
   exception
      when others =>
         Report.Failed ("Unexpected exception in Timed_Calculation_Task");
   end Timed_Calculation_Task;


          --========================================================--


   Start_Time : constant Ada.Calendar.Time := 
                         Ada.Calendar.Time_of (1901,1,1);
   Minute     : constant Duration          := 60.0;


          --========================================================--


begin  -- Main program.

   Report.Test ("C974002", "Asynchronous Select with Delay_Until");
  
   -- take care of implementations that start the clock at 1/1/01 
   delay ImpDef.Delay_For_Time_Past;


   Abortable_Part_Executed         := False;
   Triggering_Alternative_Executed := False;

   NO_DELAY_SUBTEST:

      declare
         -- Set Expiry to a time which has already passed
         Expiry : constant Ada.Calendar.Time := Start_Time; 
         Timed  : Timed_Calculation_Task;
      begin

         -- Expiry is the time to be specified in the delay_until statement
         -- of the asynchronous select. Since it has already passed, the
         -- abortable part should not execute, and the sequence of statements
         -- of the triggering alternative should be executed.

         Timed.Calculation (Time_Out => Expiry);   -- Asynchronous select
                                                   -- inside accept block.
         if Abortable_Part_Executed then
            Report.Failed ("No delay: Abortable part was executed");
         end if;

         if not Triggering_Alternative_Executed then
            Report.Failed ("No delay: triggering alternative sequence " &
                           "of statements was not executed");
         end if;
      end No_Delay_Subtest;


   Abortable_Part_Executed         := False;
   Triggering_Alternative_Executed := False;

   LONG_DELAY_SUBTEST:

      declare

         -- Quick_Calculation should finish before expiry.
         Expiry : constant Ada.Calendar.Time := 
                                            Ada.Calendar.Clock + Minute;
         Timed  : Timed_Calculation_Task;

      begin

         -- Expiry is the time to be specified in the delay_until statement
         -- of the asynchronous select. It should not pass before the abortable
         -- part completes, at which time control should return to the caller;
         -- the sequence of statements of the triggering alternative should
         -- not be executed.

         Timed.Calculation (Time_Out => Expiry);  -- Asynchronous select.

         if not Abortable_Part_Executed then
            Report.Failed ("Long delay: Abortable part was not executed");
         end if;

         if Triggering_Alternative_Executed then
            Report.Failed ("Long delay: triggering alternative sequence " &
                           "of statements was executed");
         end if;
      end Long_Delay_Subtest;


   Report.Result;

end C974002;
