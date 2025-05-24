-- C953003.A
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
--      Check that the servicing of entry queues of a protected object 
--      continues until there are no open entries with queued (or 
--      requeued) calls and that internal requeues are handled 
--      as part of a single protected operation.
--
-- TEST DESCRIPTION:
--      A number of tasks are created and blocked on a protected object
--      so that they can all be released at one time.  When released,
--      these tasks make an entry call to an entry in the Main_PO 
--      protected object.  As part of the servicing of this entry
--      call the call is passed through the remaining entries of the
--      protected object by using internal requeues.  The protected
--      object checks that no other entry call is accepted until 
--      after all the internal requeuing has completed.
--
--
-- CHANGE HISTORY:
--      12 JAN 96   SAIC    Initial version for 2.1
--
--!

with Report;
procedure C953003 is
  Verbose : constant Boolean := False;

  Order_Error : Boolean := False;

  Max_Tasks : constant := 10;  -- total number of tasks
  Max_Entries : constant := 4;  -- number of entries in Main_PO
  Note_Cnt : Integer := 0;
  Note_Order : array (1..Max_Tasks*Max_Entries) of Integer;
begin
  Report.Test ("C953003",
               "Check that the servicing of entry queues handles all" &
               " open entries as part of a single protected operation," &
               " including those resulting from an internal requeue");
  declare
     task type Assault_PO is
        entry Take_ID (Id : Integer);
     end Assault_PO;

     Marines  : array (1 .. Max_Tasks) of Assault_PO;

     protected Main_PO is
        entry E0 (Who : Integer);
     private
        entry E3 (Who : Integer);
        entry E2 (Who : Integer);
        entry E1 (Who : Integer);
        Expected_Next : Integer := 0;
     end Main_PO;

     
     protected body Main_PO is

        entry E0 (Who : Integer) when True is
        begin
           Order_Error := Order_Error or Expected_Next /= 0;
           Expected_Next := 1;
           Note_Cnt := Note_Cnt + 1;
           Note_Order (Note_Cnt) := Who;
           requeue E1;
        end E0;

        entry E1 (Who : Integer) when True is
        begin
           Order_Error := Order_Error or Expected_Next /= 1;
           Expected_Next := 2;
           Note_Cnt := Note_Cnt + 1;
           Note_Order (Note_Cnt) := Who;
           requeue E2;
        end E1;

        entry E3 (Who : Integer) when True is
        begin
           Order_Error := Order_Error or Expected_Next /= 3;
           Expected_Next := 0;
           Note_Cnt := Note_Cnt + 1;
           Note_Order (Note_Cnt) := Who;
           -- all done - return now
        end E3;

        entry E2 (Who : Integer) when True is
        begin
           Order_Error := Order_Error or Expected_Next /= 2;
           Expected_Next := 3;
           Note_Cnt := Note_Cnt + 1;
           Note_Order (Note_Cnt) := Who;
           requeue E3;
        end E2;
     end Main_PO;

     protected Holding_Pen is
        entry Wait_For_All_Present;
        entry Wait;
     private
        Open : Boolean := False;
     end Holding_Pen;

     protected body Holding_Pen is
        entry Wait_For_All_Present when Wait'Count = Max_Tasks is
        begin
           Open := True;
        end Wait_For_All_Present;

        entry Wait when Open is
        begin
           null;  -- just go
        end Wait;
     end Holding_Pen;


     task body Assault_PO is
        Me : Integer;
     begin
        accept Take_Id (Id : Integer) do
           Me := Id;
        end Take_Id;
        Holding_Pen.Wait;
        Main_PO.E0 (Me);
        if Verbose then
           Report.Comment ("task" & Integer'Image (Me) &
                           " done");
        end if;
     exception
        when others =>
           Report.Failed ("exception in task");
     end Assault_PO;

  begin   -- test encapsulation
     for I in Marines'Range loop
        Marines (I).Take_ID (100 + I);
     end loop;
     
     -- let all the tasks get blocked so we can release them all
     -- at one time
     Holding_Pen.Wait_For_All_Present; 

     -- wait for all the tasks to complete
     if Verbose then
        Report.Comment ("waiting for tasks to complete");
     end if;
  end;

  -- make sure all tasks registered their order
  if Note_Cnt /= Max_Tasks * Max_Entries then
     Report.Failed ("task registration count wrong. " &
                    Integer'Image (Note_Cnt));
  end if;

  if Order_Error then
     Report.Failed ("internal requeue not handled as part of operation");
  end if;

  if Verbose or Order_Error then
     for I in 1..Max_Tasks * Max_Entries loop
        Report.Comment ("order" & Integer'Image (I) & " is" &
                        Integer'Image (Note_Order (I))); 
     end loop;
  end if;

  Report.Result;
end C953003;
