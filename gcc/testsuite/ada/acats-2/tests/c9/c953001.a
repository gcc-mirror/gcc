-- C953001.A
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
--      Check that if the evaluation of an entry_barrier condition
--      propagates an exception, the exception Program_Error
--      is propagated to all current callers of all entries of the
--      protected object.
--
-- TEST DESCRIPTION:
--      This test declares a protected object (PO) with two entries and
--      a 5 element entry family.
--      All the entries are always closed.  However, one of the entries
--      (Oh_No) will get a constraint_error in its barrier_evaluation
--      whenever the global variable Blow_Up is true.
--      An array of tasks is created where the tasks wait on the various
--      entries of the protected object.  Once all the tasks are waiting
--      the main procedure calls the entry Oh_No and causes an exception
--      to be propagated to all the tasks.  The tasks record the fact 
--      that they got the correct exception in global variables that
--      can be checked after the tasks complete.
--
--
-- CHANGE HISTORY:
--      19 OCT 95   SAIC    ACVC 2.1
--
--!


with Report;
with ImpDef;
procedure C953001 is
    Verbose : constant Boolean := False;
    Max_Tasks : constant := 12;

      -- note status and error conditions
    Blocked_Entry_Taken : Boolean := False;
    In_Oh_No            : Boolean := False;
    Task_Passed : array (1..Max_Tasks) of Boolean := (1..Max_Tasks => False);

begin
  Report.Test ("C953001",
               "Check that an exception in an entry_barrier condition" &
               " causes Program_Error to be propagated to all current" &
               " callers of all entries of the protected object");

  declare -- test encapsulation
    -- miscellaneous values
    Cows : Integer := Report.Ident_Int (1);
    Came_Home : Integer := Report.Ident_Int (2);

    -- make the Barrier_Condition fail only when we want it to
    Blow_Up : Boolean := False;

    function Barrier_Condition return Boolean is
    begin
      if Blow_Up then
         return 5 mod Report.Ident_Int(0) = 1;
      else
         return False;
      end if;
    end Barrier_Condition;

    subtype Family_Index is Integer range 1..5;

    protected PO is
      entry Block1;
      entry Oh_No;
      entry Family (Family_Index);
    end PO;

    protected body PO is
      entry Block1 when Report.Ident_Int(0) = Report.Ident_Int(1) is
      begin
        Blocked_Entry_Taken := True;
      end Block1;

      -- barrier will get a Constraint_Error (divide by 0)
      entry Oh_No when Barrier_Condition is
      begin
        In_Oh_No := True;
      end Oh_No;

      entry Family (for Member in Family_Index) when Cows = Came_Home is
      begin
        Blocked_Entry_Taken := True;
      end Family;
    end PO;
     

    task type Waiter is
      entry Take_Id (Id : Integer);
    end Waiter;

    Bunch_of_Waiters : array (1..Max_Tasks) of Waiter;

    task body Waiter is
      Me : Integer;
      Action : Integer;
    begin
      accept Take_Id (Id : Integer) do
         Me := Id;
      end Take_Id;

      Action := Me mod (Family_Index'Last + 1);
      begin
        if Action = 0 then
          PO.Block1; 
        else
          PO.Family (Action);
        end if;
        Report.Failed ("no exception for task" & Integer'Image (Me));
      exception
         when Program_Error =>
           Task_Passed (Me) := True;
           if Verbose then
             Report.Comment ("pass for task" & Integer'Image (Me));
           end if;
         when others =>
           Report.Failed ("wrong exception raised in task" &
                          Integer'Image (Me));
      end;
    end Waiter;


  begin   -- test encapsulation
    for I in 1..Max_Tasks loop
      Bunch_Of_Waiters(I).Take_Id (I);
    end loop;

    -- give all the Waiters time to get queued
    delay 2*ImpDef.Clear_Ready_Queue;

    -- cause the protected object to fail
    begin
      Blow_Up := True;
      PO.Oh_No;
      Report.Failed ("no exception in call to PO.Oh_No");
    exception
      when Constraint_Error =>
         Report.Failed ("Constraint_Error instead of Program_Error");
      when Program_Error =>
         if Verbose then
           Report.Comment ("main exception passed");
         end if;
      when others =>
         Report.Failed ("wrong exception in main");
    end;
  end;    -- test encapsulation

  -- all the tasks have now completed.
  -- check the flags for pass/fail info
  if Blocked_Entry_Taken then
     Report.Failed ("blocked entry taken");
  end if;
  if In_Oh_No then
     Report.Failed ("entry taken with exception in barrier");
  end if;
  for I in 1..Max_Tasks loop
    if not Task_Passed (I) then
      Report.Failed ("task" & Integer'Image (I) & " did not pass");
    end if;
  end loop;

  Report.Result;
end C953001;
