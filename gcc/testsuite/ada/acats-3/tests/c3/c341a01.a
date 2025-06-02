-- C341A01.A
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
--      Check that formal parameters of a class-wide type can be passed 
--      values of any specific type within the class.
--
-- TEST DESCRIPTION:
--      Define an object of a root tagged type and of various types derived
--      from the root.  Define objects of the root class, and initialize them
--      by parameter association of objects of the specific types (root and 
--      extended types) within the class.
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
--
-- TEST FILES:
--      This test depends on the following foundation code:
--
--         F341A00.A
--
--      The following files comprise this test:
--
--      => C341A01.A
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

procedure C341A01 is

   package Bank              renames F341A00_0;
   use type Bank.Dollar_Amount;
   package Checking          renames F341A00_1;
   package Interest_Checking renames F341A00_2;

   Max_Accts    : constant := 3;
   Bank_Balance : Bank.Dollar_Amount := 0.00;

   -- Initialize objects of specific tagged types.
   B_Acct  : Bank.Account              := (Current_Balance => 10.00); 
   C_Acct  : Checking.Account          := (100.00, 10.00); 
   IC_Acct : Interest_Checking.Account := (1000.00, 10.00, 0.030);

   -- Define and initialize (by parameter association) objects of class-wide 
   -- type originating from the root type (Bank.Account).

   -- Define an account auditing procedure with a class-wide
   -- variable that can hold a value of any object within the class.
   procedure Audit (Next_Account : Bank.Account'Class) is
   begin
      Bank_Balance := Bank_Balance + Next_Account.Current_Balance;
   end Audit;


begin  -- C341A01

   Report.Test ("C341A01", "Check that objects of a class-wide type can " &
                           "be initialized, by direct assignment, to a "  &
                           "value of any specific type within the class" ); 

   -- Perform nightly audit of total funds on deposit in bank.
   Audit (B_Acct);
   Audit (C_Acct);
   Audit (IC_Acct);

   if Bank_Balance /= 1110.00 then
      Report.Failed ("Class-wide object processing failed");
   end if;

   Report.Result;

end C341A01;
