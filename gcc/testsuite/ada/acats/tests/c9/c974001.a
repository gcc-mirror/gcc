-- C974001.A
--
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
--      Check that the abortable part of an asynchronous select statement
--      is aborted if it does not complete before the triggering statement
--      completes, where the triggering statement is a delay_relative
--      statement and check that the sequence of statements of the triggering 
--      alternative is executed after the abortable part is left.
--
-- TEST DESCRIPTION:
--      Declare a task with an accept statement containing an asynchronous
--      select with a delay_relative triggering statement. Parameterize
--      the accept statement with the time to be used in the delay. Simulate a
--      time-consuming calculation by declaring a procedure containing an
--      infinite loop. Call this procedure in the abortable part.
--
--      The delay will expire before the abortable part completes, at which
--      time the abortable part is aborted, and the sequence of statements
--      following the triggering statement is executed.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;
with ImpDef;

procedure C974001 is


          --========================================================--

   -- Medium length delay
   Allotted_Time : constant Duration :=  ImpDef.Switch_To_New_Task;

   Calculation_Canceled : exception;


   Count : Integer := 1234;                          
                                                    
   procedure Lengthy_Calculation is
   begin
      -- Simulate a non-converging calculation.
      loop                                           -- Infinite loop.
         Count := (Count + 1) mod 10;
         delay ImpDef.Minimum_Task_Switch;           -- allow other task 
      end loop;
   end Lengthy_Calculation;


          --========================================================--


   task type Timed_Calculation is
      entry Calculation (Time_Limit : in Duration);
   end Timed_Calculation;


   task body Timed_Calculation is
   --
   begin
      loop
         select
            accept Calculation (Time_Limit : in Duration) do

               --                                    --
               -- Asynchronous select is tested here --
               --                                    --

               select
                  delay Time_Limit;           -- Time_Limit is not up yet, so
                                              -- Lengthy_Calculation starts.

                  raise Calculation_Canceled; -- This is executed after
                                              -- Lengthy_Calculation aborted.
               then abort
                  Lengthy_Calculation;        -- Delay expires before complete,
                                              -- so this call is aborted.

                  -- Check that the whole of the abortable part is aborted,
                  -- not just the statement in the abortable part that was
                  -- executing at the time
                  Report.Failed ("Abortable part not aborted");

               end select;

               Report.Failed ("Triggering alternative sequence of " &
                              "statements not executed");

            exception    -- New Ada 9x: handler within accept
               when Calculation_Canceled =>
                  if Count = 1234 then
                     Report.Failed ("Abortable part did not execute");
                  end if;
            end Calculation;
         or
            terminate;
         end select;
      end loop;
   exception
      when others => 
         Report.Failed ("Unexpected exception in Timed_Calculation task");
   end Timed_Calculation;


          --========================================================--


begin  -- Main program.

   Report.Test ("C974001", "Asynchronous Select: Trigger is delay_relative" &
                           " which completes before abortable part");

   declare
      Timed : Timed_Calculation;  -- Task.
   begin
      Timed.Calculation (Time_Limit => Allotted_Time); -- Asynchronous select
                                                       -- inside accept block.
   exception
      when Calculation_Canceled => 
         null; -- expected behavior
   end;

   Report.Result;

end C974001;
