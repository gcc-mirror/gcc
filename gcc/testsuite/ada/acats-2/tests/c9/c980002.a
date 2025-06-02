-- C980002.A
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
--      Check that aborts are deferred during protected actions.
--
-- TEST DESCRIPTION:
--      This test uses an asynchronous transfer of control to attempt
--      to abort a protected operation.  The protected operation
--      includes several requeues to check that the requeue does not
--      allow the abort to occur.
--
--
-- CHANGE HISTORY:
--      30 OCT 95   SAIC    ACVC 2.1
--
--!

with Report;
procedure C980002 is

  Max_Checkpoints : constant := 7;
  type Checkpoint_ID is range 1..Max_Checkpoints;
  type Points_Array is array (Checkpoint_ID) of Boolean;
begin
  Report.Test ("C980002",
               "Check that aborts are deferred during a protected action" &
               " including requeues");

  declare -- test encapsulation

    protected Checkpoint is
       procedure Got_Here (Id : Checkpoint_ID);
       function Results return Points_Array;
    private
       Reached_Points : Points_Array := (others => False);
    end Checkpoint;

    protected body Checkpoint is
       procedure Got_Here (Id : Checkpoint_ID) is
       begin
          Reached_Points (Id) := True;
       end Got_Here;

       function Results return Points_Array is
       begin
          return Reached_Points;
       end Results;
    end Checkpoint;


    protected Start_Here is
       entry AST_Waits_Here;
       entry Start_PO;
    private
       Open : Boolean := False;
       entry First_Stop;
    end Start_Here;

    protected Middle_PO is
       entry Stop_1;
       entry Stop_2;
    end Middle_PO;

    protected Final_PO is
       entry Final_Stop;
    end Final_PO;


    protected body Start_Here is
       entry AST_Waits_Here when Open is
       begin
          null;
       end AST_Waits_Here;

       entry Start_PO when True is
       begin
          Open := True;
          Checkpoint.Got_Here (1);
          requeue First_Stop; 
       end Start_PO;

       -- make sure the AST has been accepted before continuing
       entry First_Stop when AST_Waits_Here'Count = 0 is
       begin
          Checkpoint.Got_Here (2);
          requeue Middle_PO.Stop_1;
       end First_Stop;
    end Start_Here;

    protected body Middle_PO is
       entry Stop_1 when True is
       begin
          Checkpoint.Got_Here (3);
          requeue Stop_2;
       end Stop_1;

       entry Stop_2 when True is
       begin
          Checkpoint.Got_Here (4);
          requeue Final_PO.Final_Stop;
       end Stop_2;
    end Middle_PO;

    protected body Final_PO is
       entry Final_Stop when True is
       begin
          Checkpoint.Got_Here (5);
       end Final_Stop;
    end Final_PO;


  begin   -- test encapsulation
    select
       Start_Here.AST_Waits_Here;
       Checkpoint.Got_Here (6);
    then abort
       Start_Here.Start_PO;
       delay 0.0;  -- abort completion point
       Checkpoint.Got_Here (7);
    end select;

    Check_The_Results: declare
       Chk : constant Points_Array := Checkpoint.Results;
       Expected : constant Points_Array :=  (1..6 => True,
                                             7    => False);
    begin
       for I in Checkpoint_ID loop
          if Chk (I) /= Expected (I) then
             Report.Failed ("checkpoint error" &
                            Checkpoint_ID'Image (I) &
                            " actual is " &
                            Boolean'Image (Chk(I)));
          end if;         
       end loop; 
    end Check_The_Results;
  exception
    when others =>
       Report.Failed ("unexpected exception");
  end;    -- test encapsulation
  
  Report.Result;
end C980002;
