-- C392A01.A
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
 --      Check that the use of a class-wide formal parameter allows for the 
 --      proper dispatching of objects to the appropriate implementation of 
 --      a primitive operation.  Check this for the root tagged type defined
 --      in a package, and the extended type is defined in that same package.
 --
 -- TEST DESCRIPTION:
 --      Declare a root tagged type, and some associated primitive operations.
 --      Extend the root type, and override one or more primitive operations, 
 --      inheriting the other primitive operations from the root type.
 --      Derive from the extended type, again overriding some primitive
 --      operations and inheriting others (including some that the parent 
 --      inherited).
 --      Define a subprogram with a class-wide parameter, inside of which is a 
 --      call on a dispatching primitive operation.  These primitive operations
 --      modify global variables (the class-wide parameter has mode IN).
 --     
 --     
 --     
 -- The following hierarchy of tagged types and primitive operations is 
 -- utilized in this test:
 --
 --    type Bank_Account (root)
 --            |
 --            | Operations
 --            |   Increment_Bank_Reserve
 --            |   Assign_Representative
 --            |   Increment_Counters
 --            |   Open
 --            |
 --    type Savings_Account (extended from Bank_Account)
 --            |
 --            | Operations
 --            |   (Increment_Bank_Reserve) (inherited)
 --            |   Assign_Representative    (overridden)
 --            |   Increment_Counters       (overridden)
 --            |   Open                     (overridden)
 --            |
 --    type Preferred_Account (extended from Savings_Account)
 --            |
 --            | Operations
 --            |   (Increment_Bank_Reserve) (inherited twice - Bank_Acct.)
 --            |   (Assign_Representative)  (inherited - Savings_Acct.)
 --            |   Increment_Counters       (overridden)
 --            |   Open                     (overridden)
 -- 
 --
 -- In this test, we are concerned with the following selection of dispatching
 -- calls, accomplished with the use of a Bank_Account'Class IN procedure 
 -- parameter :
 --
 --                       \ Type
 --               Prim. Op \  Bank_Account  Savings_Account Preferred_Account
 --                         \------------------------------------------------ 
 --   Increment_Bank_Reserve|      X               X               X
 --   Assign_Representative |                      X
 --   Increment_Counters    |      X               X               X
 --
 --
 --
 -- The location of the declaration and derivation of the root and extended
 -- types will be varied over a series of tests.  Locations of declaration
 -- and derivation for a particular test are marked with an asterisk (*).
 --
 -- Root type:
 --       
 --    *  Declared in package.
 --       Declared in generic package.
 --
 -- Extended types:
 --
 --    *  Derived in parent location.
 --       Derived in a nested package.
 --       Derived in a nested subprogram.
 --       Derived in a nested generic package.
 --       Derived in a separate package.
 --       Derived in a separate visible child package.
 --       Derived in a separate private child package.
 --
 -- Primitive Operations:
 --
 --    *  Procedures with same parameter profile.
 --       Procedures with different parameter profile.
 --       Functions with same parameter profile.
 --       Functions with different parameter profile.
 --       Mixture of Procedures and Functions.
 --
 --
 -- TEST FILES:
 --      This test depends on the following foundation code:
 --
 --         F392A00.A
 --
 --      The following files comprise this test:
 --
 --      => C392A01.A
 --
 --
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
 --!
 
 with F392A00;         -- package Accounts
 with Report;
 
 procedure C392A01 is
 
    package Accounts renames F392A00;
 
    -- Declare account objects.
 
    B_Account : Accounts.Bank_Account;
    S_Account : Accounts.Savings_Account;
    P_Account : Accounts.Preferred_Account;
 
    -- Procedures to operate on accounts.
    -- Each uses a class-wide IN parameter, as well as a call to a
    -- dispatching operation.
 
    -- Procedure Tabulate_Account performs a dispatching call on a primitive
    -- operation that has been overridden for each of the extended types.
 
    procedure Tabulate_Account (Acct : in Accounts.Bank_Account'Class) is
    begin
       Accounts.Increment_Counters (Acct);   -- Dispatch according to tag.
    end Tabulate_Account;
 
 
    -- Procedure Accumulate_Reserve performs a dispatching call on a
    -- primitive operation that has been defined for the root type and 
    -- inherited by each derived type.
 
    procedure Accumulate_Reserve (Acct : in Accounts.Bank_Account'Class) is
    begin
       Accounts.Increment_Bank_Reserve (Acct);   -- Dispatch according to tag.
    end Accumulate_Reserve;
 
 
    -- Procedure Resolve_Dispute performs a dispatching call on a primitive
    -- operation that has been defined in the root type, overridden in the
    -- first derived extended type, and inherited by the subsequent extended
    -- type.
 
    procedure Resolve_Dispute (Acct : in Accounts.Bank_Account'Class) is
    begin
       Accounts.Assign_Representative (Acct);   -- Dispatch according to tag.
    end Resolve_Dispute;
 
 
 
 begin  -- Main test procedure.
 
    Report.Test ("C392A01", "Check that the use of a class-wide parameter "   &
                             "allows for proper dispatching where root type " &
                             "and extended types are declared in the same "   &
                             "package" );
    
    Bank_Account_Subtest:
    declare
      use Accounts;
    begin
       Accounts.Open (B_Account);
 
       -- Demonstrate class-wide parameter allowing dispatch by a primitive
       -- operation that has been defined for this specific type.
       Accumulate_Reserve (Acct => B_Account);
       Tabulate_Account (B_Account);
 
       if (Accounts.Bank_Reserve /= Accounts.Opening_Balance) or
          (Accounts.Number_Of_Accounts (Bank) /= 1)           or
          (Accounts.Number_Of_Accounts (Total) /= 1)
       then
          Report.Failed ("Failed in Bank_Account_Subtest");
       end if;
 
    end Bank_Account_Subtest;
 
 
    Savings_Account_Subtest:
    declare
      use Accounts;
    begin
       Accounts.Open (Acct => S_Account);
 
       -- Demonstrate class-wide parameter allowing dispatch by a primitive
       -- operation that has been inherited by this extended type.
       Accumulate_Reserve (Acct => S_Account);
 
       -- Demonstrate class-wide parameter allowing dispatch by a primitive
       -- operation that has been overridden for this extended type.
       Resolve_Dispute  (Acct => S_Account);
       Tabulate_Account (S_Account);
 
       if Accounts.Bank_Reserve /= (3.0 * Accounts.Opening_Balance) or
          Accounts.Daily_Representative /= Accounts.Manager         or
          Accounts.Number_Of_Accounts (Savings) /= 1                or
          Accounts.Number_Of_Accounts (Total) /= 2
       then
          Report.Failed ("Failed in Savings_Account_Subtest");
       end if;
 
    end Savings_Account_Subtest;
 
 
    Preferred_Account_Subtest:
    declare
      use Accounts;
    begin
       Accounts.Open (P_Account);
 
       -- Verify that the correct implementation of Open (overridden) was 
       -- used for the Preferred_Account object.
       if not Accounts.Verify_Open (P_Account) then
          Report.Failed ("Incorrect values for init. Preferred Acct object");
       end if;
 
       -- Demonstrate class-wide parameter allowing dispatch by a primitive
       -- operation that has been twice inherited by this extended type.
       Accumulate_Reserve (Acct => P_Account);
 
       -- Demonstrate class-wide parameter allowing dispatch by a primitive
       -- operation that has been overridden for this extended type (the
       -- operation was overridden by its parent type as well).
       Tabulate_Account (P_Account);
       
       if Accounts.Bank_Reserve /= 1300.00             or
          Accounts.Number_Of_Accounts (Preferred) /= 1 or
          Accounts.Number_Of_Accounts (Total) /= 3
       then
          Report.Failed ("Failed in Preferred_Account_Subtest");
       end if;
 
    end Preferred_Account_Subtest;
 
 
    Report.Result;
 
 end C392A01;
 
