-- C341A04.A
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
 --      Check that class-wide objects can be initialized using allocation. 
 --
 -- TEST DESCRIPTION:
 --      Declare access types that refer to class-wide types, one with basis
 --      of the root type, another with basis of a type extended from the root.
 --      Declare objects of these access types, and allocate class-wide
 --      objects, initialized to values of specific types within the particular
 --      classes.
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
 --      => C341A04.A
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
 
 procedure C341A04 is
 
    package Bank              renames F341A00_0;
    package Checking          renames F341A00_1;
    package Interest_Checking renames F341A00_2;
 
    use type Bank.Dollar_Amount;
 
    Max_Accts    : constant := 3;
    Bank_Balance : Bank.Dollar_Amount := 0.00;
 
    -- Define access types referring to class of types rooted at 
    -- Bank.Account (root). 
 
    type Bank_Account_Pointer is access Bank.Account'Class;
 
    --
    -- Define class-wide objects, initializing them through allocation.
    --
 
    -- Initialized to specific type that is basis of class.
    Bank_Acct : Bank_Account_Pointer :=
      new Bank.Account'(Current_Balance => 10.00);
 
    -- Initialized to specific type that has been extended from the basis
    -- of the class.
    Checking_Acct : Bank_Account_Pointer :=
      new Checking.Account'(Current_Balance => 100.00,
                            Overdraft_Fee   =>  10.00);
 
    -- Initialized to specific type that has been twice extended from the
    -- basis of the class.
    IC_Acct : Bank_Account_Pointer := 
      new Interest_Checking.Account'(Current_Balance => 1000.00,
                                     Overdraft_Fee   =>   10.00,
                                     Rate            =>    0.030);
 
    -- Declare and initialize array of pointers to objects of
    -- Bank.Account'Class.
 
    Accounts : array (1 .. Max_Accts) of Bank_Account_Pointer :=
      (Bank_Acct, Checking_Acct, IC_Acct);
 
 
    -- Audit will process any account object within Bank.Account'Class.
 
    function Audit (Ptr : Bank_Account_Pointer) return Bank.Dollar_Amount is
    begin
       return (Ptr.Current_Balance);
    end Audit;
 
 
 begin  -- C341A04
 
    Report.Test ("C341A04", "Check that class-wide objects were " &
                            "successfully initialized using allocation" );
 
    for i in 1 .. Max_Accts loop
       Bank_Balance := Bank_Balance + Audit (Accounts(i));
    end loop;
 
    if Bank_Balance /= 1110.00 then
       Report.Failed ("Failed class-wide object allocation");
    end if;
 
    Report.Result;
 
 end C341A04;
 
