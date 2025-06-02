-- C974014.A 
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
--      Check that if the triggering alternative of an asynchronous select 
--      statement is a delay and the abortable part completes before the delay
--      expires then the delay is cancelled and the optional statements in the
--      triggering part are not performed.  In particular, check the case of
--      the ATC in non-tasking code.
--
-- TEST DESCRIPTION:
--      A fraction of in-line code is simulated.  An asynchronous select
--      is used with a triggering delay of several minutes.  The abortable
--      part, which is simulating a very lengthy, time consuming procedure
--      actually returns almost immediately thus ensuring that it completes
--      first.  At the conclusion, if a substantial amount of time has passed
--      the delay is assumed not to have been cancelled. 
--      (based on example in LRM 9.7.4)
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


with Report;
with Ada.Calendar;
         
procedure C974014 is

   function "-" (Left, Right : Ada.Calendar.Time)
                              return Duration      renames Ada.Calendar."-";

   TC_Start_Time      : Ada.Calendar.Time := Ada.Calendar.Clock;
   TC_Elapsed_Time    : duration;

   Maximum_Allowable_Time : duration := 300.0; -- for Calculate_Gamma_Function

begin

   Report.Test ("C974014", "ATC: When abortable part completes before " &
                             "a triggering delay, check that the delay " &
                             "is cancelled & optional statements " &
                             "are not performed"); 

   declare  -- encapsulate test code

      type Gamma_Index is digits 5;   -- float precision

      -- (These two fields are assumed filled elsewhere)
      Input_Field, Result_of_Beta : Gamma_Index;   

      -- Notify and take corrective action in the event that
      -- the procedure Calculate_Gamma_Function does not converge.
      --
      procedure Non_Convergent is
      begin
         null;  -- stub

         Report.Failed ("Optional statements in triggering part" &
                                    " were performed");       
      end Non_Convergent;


      --  This is a very time consuming calculation.  It is possible,
      --  that, with certain parameters, it will not converge.  If it
      --  runs for more than Maximum_Allowable_Time it is considered
      --  not to be convergent and should be aborted.
      --
      Procedure Calculate_Gamma_Function (X, Y : Gamma_Index) is
      begin
         null;  -- Stub
         --
      end Calculate_Gamma_Function;

   begin  -- declare

      -- .....  Isolated segment of inline code

      -- Now Print Gamma Function (abort and display if not convergent)
      --
      select
         delay Maximum_Allowable_Time;  -- for Calculate_Gamma_Function
         Non_Convergent;   -- Display error and flag result as failed

      then abort
         Calculate_Gamma_Function (Input_Field, Result_of_Beta);
      end select;

      -- .....  End of Isolated segment of inline code

   end; -- declare
   
   TC_Elapsed_Time := Ada.Calendar.Clock - TC_Start_Time;

   -- Note: We are not checking for "cancellation within a reasonable time",
   -- we are checking for cancellation/non-cancellation of the delay.  We
   -- use a number which, if exceeded, means that the delay was not
   -- cancelled and has proceeded to  full term.
   --
   if ( TC_Elapsed_Time > Maximum_Allowable_Time/2 ) then
      -- Test time exceeds a reasonable value.
      Report.Failed ("Triggering delay statement was not cancelled");
   end if;


   Report.Result;

end C974014;
