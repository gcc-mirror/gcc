-- C953002.A
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
--      continues until there are no open entries with queued calls
--      and that this takes place as part of a single protected 
--      operation.
--
-- TEST DESCRIPTION:
--      This test enqueues a bunch of tasks on the entries of the
--      protected object Main_PO.  At the same time another bunch of
--      of tasks are queued on the single entry of protected object
--      Holding_Pen.
--      Once all the tasks have had time to block, the main procedure
--      opens all the entries for Main_PO by calling the 
--      Start_Protected_Operation protected procedure.  This should
--      process all the pending callers as part of a single protected
--      operation.
--      During this protected operation, the entries of Main_PO release
--      the tasks blocked on Holding_Pen by calling the protected 
--      procedure Release.
--      Once released from Holding_Pen, the task immediately calls
--      an entry in Main_PO.
--      These new calls should not gain access to Main_PO until
--      the initial protected operation on that object completes.
--      The order in which the entry calls on Main_PO are taken is
--      recorded in a global array and checked after all the tasks
--      have terminated.
--
--
-- CHANGE HISTORY:
--      25 OCT 95   SAIC    ACVC 2.1
--      15 JAN 95   SAIC    Fixed deadlock problem.
--
--!

with Report;
procedure C953002 is
  Verbose : constant Boolean := False;

  Half_Tasks : constant := 15;   -- how many tasks of each group 
  Max_Tasks : constant := Half_Tasks * 2;  -- total number of tasks

  Note_Order : array (1..Max_Tasks) of Integer := (1..Max_Tasks => 0);
  Note_Cnt : Integer := 0;
begin
  Report.Test ("C953002",
               "Check that the servicing of entry queues handles all" &
               " open entries as part of a single protected operation");
  declare
     task type Assault_PO is
        entry Take_ID (Id : Integer);
     end Assault_PO;

     First_Wave  : array (1 .. Half_Tasks) of Assault_PO;
     Second_Wave : array (1 .. Half_Tasks) of Assault_PO;

     protected Main_PO is
        entry E0 (Who : Integer);
        entry E1 (Who : Integer);
        entry E2 (Who : Integer);
        entry E3 (Who : Integer);
        entry All_Present;
        procedure Start_Protected_Operation;
     private
        Open : Boolean := False;
     end Main_PO;

     protected Holding_Pen is
        -- Note that Release is called by tasks executing in 
        -- the protected object Main_PO.
        entry Wait (Who : Integer);
        entry All_Present;
        procedure Release;
     private
        Open : Boolean := False;
     end Holding_Pen;

     
     protected body Main_PO is
        procedure Start_Protected_Operation is
        begin
           Open := True;
           -- at this point all the First_Wave tasks are
           -- waiting at the entries and all of them should
           -- be processed as part of the protected operation.
        end Start_Protected_Operation;

        entry All_Present when E0'Count + E1'Count + E2'Count + E3'Count =
                               Max_Tasks / 2  is
        begin
           null;   -- all tasks are waiting
        end All_Present; 

        entry E0 (Who : Integer) when Open is
        begin
           Holding_Pen.Release;
           -- note the order in which entry calls are handled.
           Note_Cnt := Note_Cnt + 1;
           Note_Order (Note_Cnt) := Who;
        end E0;

        entry E1 (Who : Integer) when Open is
        begin
           Holding_Pen.Release;
           Note_Cnt := Note_Cnt + 1;
           Note_Order (Note_Cnt) := Who;
        end E1;

        entry E2 (Who : Integer) when Open is
        begin
           Holding_Pen.Release;
           Note_Cnt := Note_Cnt + 1;
           Note_Order (Note_Cnt) := Who;
        end E2;

        entry E3 (Who : Integer) when Open is
        begin
           Holding_Pen.Release;
           Note_Cnt := Note_Cnt + 1;
           Note_Order (Note_Cnt) := Who;
        end E3;
     end Main_PO;


     protected body Holding_Pen is
        procedure Release is
        begin
           Open := True;
        end Release;

        entry All_Present when Wait'Count = Max_Tasks / 2 is
        begin
           null;  -- all tasks waiting
        end All_Present;

        entry Wait (Who : Integer) when Open is
        begin
           null;  -- unblock the task
        end Wait;
     end Holding_Pen;

     task body Assault_PO is
        Me : Integer;
     begin
        accept Take_Id (Id : Integer) do
           Me := Id;
        end Take_Id;
        if Me >= 200 then
           Holding_Pen.Wait (Me);
        end if;
        case Me mod 4 is
           when 0 => Main_PO.E0 (Me);
           when 1 => Main_PO.E1 (Me);
           when 2 => Main_PO.E2 (Me);
           when 3 => Main_PO.E3 (Me);
           when others => null;  -- cant happen
        end case;
        if Verbose then
           Report.Comment ("task" & Integer'Image (Me) &
                           " done");
        end if;
     exception
        when others =>
           Report.Failed ("exception in task");
     end Assault_PO;

  begin   -- test encapsulation
     for I in First_Wave'Range loop
        First_Wave (I).Take_ID (100 + I);
     end loop;
     for I in Second_Wave'Range loop
        Second_Wave (I).Take_ID (200 + I);
     end loop;
     
     -- let all the tasks get blocked
     Main_PO.All_Present;
     Holding_Pen.All_Present; 

     -- let the games begin
     if Verbose then
        Report.Comment ("starting protected operation");
     end if;
     Main_PO.Start_Protected_Operation;

     -- wait for all the tasks to complete
     if Verbose then
        Report.Comment ("waiting for tasks to complete");
     end if;
  end;

  -- make sure all tasks registered their order
  if Note_Cnt /= Max_Tasks then
     Report.Failed ("task registration count wrong. " &
                    Integer'Image (Note_Cnt));
  end if;

  -- check the order in which entries were handled.
  -- all the 100 level items should be handled as part of the
  -- first protected operation and thus should be completed
  -- before any 200 level item.

  if Verbose then
     for I in 1..Max_Tasks loop
        Report.Comment ("order" & Integer'Image (I) & " is" &
                        Integer'Image (Note_Order (I))); 
     end loop;
  end if;
  for I in 2 .. Max_Tasks loop
     if Note_Order (I)   <  200 and
        Note_Order (I-1) >= 200 then
        Report.Failed ("protected operation failure" &
                       Integer'Image (Note_Order (I-1)) &
                       Integer'Image (Note_Order (I)));
     end if;
  end loop;
  
  Report.Result;
end C953002;
