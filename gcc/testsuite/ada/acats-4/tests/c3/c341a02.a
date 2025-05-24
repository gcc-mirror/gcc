-- C341A02.A
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
 --      Check that class-wide objects can be reassigned with objects from 
 --      the same specific type used to initialize them.
 --
 -- TEST DESCRIPTION:
 --      Define new objects of specific types from within a class. Reassign
 --      previously declared class-wide objects with the new specific type 
 --      objects. Check that new assignments were performed.
 --
 --      The particular root and extended types used in this abstraction are
 --      defined in foundation code (F341A00.A), and are graphically displayed
 --      as follows:
 --
 --           package Bank
 --              type Account
 --                  |
 --                  |
 --                  |
 --               package Checking
 --                  type Account
 --                      |
 --                      |
 --                      | 
 --                   package Interest_Checking
 --                          type Account
 --
 -- TEST FILES:
 --      This test depends on the following foundation code:
 --
 --         F341A00.A
 --
 --      The following files comprise this test:
 --
 --      => C341A02.A
 --
 --
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
 --!
 
 with F341A00_0;            -- package Bank
 with F341A00_1;            -- package Checking
 with F341A00_2;            -- package Interest_Checking
 with Report;
 
 procedure C341A02 is
 
    package Bank              renames F341A00_0;
    package Checking          renames F341A00_1;
    package Interest_Checking renames F341A00_2;
 
    Max_Accts    : constant := 3;
    Bank_Balance : Bank.Dollar_Amount := 0.00;
 
    -- Define and initialize objects of specific types.
    B_Acct      : aliased Bank.Account := (Current_Balance => 10.00); 
    C_Acct      : aliased Checking.Account          := (100.00, 10.00); 
    IC_Acct     : aliased Interest_Checking.Account := (1000.00, 10.00, 0.030);
    New_B_Acct  : aliased Bank.Account := (Current_Balance => 20.00);
    New_C_Acct  : aliased Checking.Account          := (200.00,  20.00);
    New_IC_Acct : aliased Interest_Checking.Account := (2000.00, 20.00, 0.060);
 
 
    -- Define and initialize (by direct assignment) objects of a class-wide 
    -- type originating from the root type (Bank.Account).
    
    type ATM_Card is access all Bank.Account'Class;
 
    Accounts : array (1 .. Max_Accts) of ATM_Card := 
      (1 => B_Acct'Access, 2 => C_Acct'Access, 3 => IC_Acct'Access);
 
    New_Accounts : array (1 .. Max_Accts) of ATM_Card := 
      (1 => New_B_Acct'Access,
       2 => New_C_Acct'Access,
       3 => New_IC_Acct'Access);
 
    -- Define an account auditing procedure with a class-wide
    -- variable that can hold a value of any object within the class,
    -- and once initialized, can hold other values of the same specific type.
 
    procedure Audit (Num : in     integer;
                     Amt :    out Bank.Dollar_Amount) is
       Account_Being_Audited : Bank.Account'Class := Accounts(Num).all;
       use type Bank.Dollar_Amount;
    begin
       Amt := Account_Being_Audited.Current_Balance;
       -- Reassign class-wide variable to another object of the type used to
       -- initialize it.
       Account_Being_Audited := New_Accounts(Num).all;
       Amt := Amt + Account_Being_Audited.Current_Balance;  -- Reading OUT
    end Audit;                                              -- parameter.
 
 
 begin 
 
    Report.Test ("C341A02", "Check that class-wide objects can be "  &
                            "reassigned with objects from the same " &
                            "specific type used to initialize them" );
    Night_Audit:
    declare
       use type Bank.Dollar_Amount;
       Acct_Value : Bank.Dollar_Amount := 0.00;
    begin
       -- Perform nightly audit of total funds on deposit in bank.
       for i in 1 .. Max_Accts loop
          Audit (i, Acct_Value);
          Bank_Balance := Bank_Balance + Acct_Value;
       end loop;
 
       if Bank_Balance /= 3330.00 then
          Report.Failed ("Class-wide object processing failed");
       end if;
 
    end Night_Audit;
 
    Report.Result;
 
 end C341A02;
 
