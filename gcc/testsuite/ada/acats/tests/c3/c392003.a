-- C392003.A
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
--      a primitive operation.  Check this where the root tagged type is
--      defined in a package, and the extended type is defined in a nested
--      package.
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
--   Increment_Bank_Reserve|      X                               X
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
--       Derived in parent location.
--    *  Derived in a nested package.
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
--    *  Functions with same parameter profile.
--       Functions with different parameter profile.
--    *  Mixture of Procedures and Functions.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!
 
 
 with Report;
 
 procedure C392003 is
 
       --
       -- Types and subtypes.
       --
 
       type Dollar_Amount  is new float;
       type Interest_Rate  is delta 0.001 range 0.000 .. 1.000;
       type Account_Types  is (Bank, Savings, Preferred, Total);
       type Account_Counter is array (Account_Types) of integer;
       type Account_Rep is (President, Manager, New_Account_Manager, Teller);
 
       --
       -- Constants.
       --
 
       Opening_Balance           : constant Dollar_Amount := 100.00;
       Current_Rate              : constant Interest_Rate := 0.030;
       Preferred_Minimum_Balance : constant Dollar_Amount := 1000.00;
 
       --
       -- Global Variables
       --
 
       Bank_Reserve         : Dollar_Amount   := 0.00;
       Daily_Representative : Account_Rep     := New_Account_Manager;
       Number_Of_Accounts   : Account_Counter := (Bank      => 0,
                                                  Savings   => 0,
                                                  Preferred => 0,
                                                  Total     => 0);
 
    -- Root tagged type and primitive operations declared in internal 
    -- package (Accounts).
    -- Extended types (and primitive operations) derived in nested packages.
 
      --=================================================================--
 
    package Accounts is
 
       --
       -- Root account type and primitive operations.
       --
 
       -- Root type.
 
       type Bank_Account is tagged
          record
             Balance : Dollar_Amount;
          end record;
 
       -- Primitive operations of Bank_Account.
 
       function  Increment_Bank_Reserve (Acct : in     Bank_Account) 
         return Dollar_Amount;
       function Assign_Representative   (Acct : in     Bank_Account)
         return Account_Rep;
       procedure Increment_Counters     (Acct : in     Bank_Account);
       procedure Open                   (Acct : in out Bank_Account);
 
      --=================================================================--
                           
       package S_And_L is 
 
          -- Declare extended type in a nested package.
 
          type Savings_Account is new Bank_Account with
          record
             Rate : Interest_Rate;
          end record;
 
          -- Function Increment_Bank_Reserve inherited from 
          -- parent (Bank_Account).
 
          -- Primitive operations (Overridden).
          function Assign_Representative (Acct : in     Savings_Account)
            return Account_Rep;
          procedure Increment_Counters   (Acct : in     Savings_Account);
          procedure Open                 (Acct : in out Savings_Account);
                           
 
      --=================================================================--
                           
          package Premium is 
 
             -- Declare further extended type in a nested package.
 
             type Preferred_Account is new Savings_Account with
             record
                Minimum_Balance : Dollar_Amount;
             end record;
 
             -- Function Increment_Bank_Reserve inherited twice.
             -- Function Assign_Representative inherited from parent 
             --   (Savings_Account).
 
             -- Primitive operation (Overridden).
             procedure Increment_Counters (Acct : in     Preferred_Account);
             procedure Open               (Acct : in out Preferred_Account);
 
             -- Function used to verify Open operation for Preferred_Account 
             -- objects.
             function Verify_Open (Acct : in Preferred_Account) return Boolean;
 
          end Premium;
 
       end S_And_L;
 
    end Accounts;
 
      --=================================================================--
 
    package body Accounts is
 
       --
       -- Primitive operations for Bank_Account.
       --
 
       function Increment_Bank_Reserve (Acct : in Bank_Account) 
         return Dollar_Amount is
       begin
          return (Bank_Reserve + Acct.Balance);
       end Increment_Bank_Reserve;
 
       function Assign_Representative (Acct : in Bank_Account) 
         return Account_Rep is
       begin
          return Account_Rep'(Teller);
       end Assign_Representative;
 
       procedure Increment_Counters (Acct : in Bank_Account) is
       begin
          Number_Of_Accounts (Bank)  := Number_Of_Accounts (Bank) + 1;
          Number_Of_Accounts (Total) := Number_Of_Accounts (Total) + 1;
       end Increment_Counters;
 
       procedure Open (Acct : in out Bank_Account) is
       begin
          Acct.Balance := Opening_Balance;
       end Open;
 
      --=================================================================--
 
       package body S_And_L is
 
          --
          -- Overridden operations for Savings_Account type.
          --
 
          function Assign_Representative (Acct : in Savings_Account) 
            return Account_Rep is
          begin
             return (Manager);
          end Assign_Representative;
 
          procedure Increment_Counters (Acct : in Savings_Account) is
          begin
             Number_Of_Accounts (Savings) := Number_Of_Accounts (Savings) + 1;
             Number_Of_Accounts (Total)   := Number_Of_Accounts (Total) + 1;
          end Increment_Counters;
 
          procedure Open (Acct : in out Savings_Account) is
          begin
             Open (Bank_Account(Acct));
             Acct.Rate := Current_Rate;
             Acct.Balance := 2.0 * Opening_Balance;
          end Open;
      
      --=================================================================--
 
          package body Premium is
 
             --
             -- Overridden operations for Preferred_Account type.
             --
                      
             procedure Increment_Counters (Acct : in Preferred_Account) is
             begin
                Number_Of_Accounts (Preferred) := 
                  Number_Of_Accounts (Preferred) + 1;
                Number_Of_Accounts (Total)     := 
                  Number_Of_Accounts (Total) + 1;
             end Increment_Counters;
 
             procedure Open (Acct : in out Preferred_Account) is
             begin
                Open (Savings_Account(Acct));
                Acct.Minimum_Balance := Preferred_Minimum_Balance;
                Acct.Balance := Acct.Minimum_Balance;
             end Open;
 
             --
             -- Function used to verify Open operation for Preferred_Account 
             -- objects.
             --
 
             function Verify_Open (Acct : in Preferred_Account) 
               return Boolean is
             begin                                                    
                return (Acct.Balance         = Preferred_Minimum_Balance and
                        Acct.Rate            = Current_Rate              and
                        Acct.Minimum_Balance = Preferred_Minimum_Balance);
             end Verify_Open;
 
          end Premium;
  
       end S_And_L;
 
    end Accounts;
 
      --=================================================================--
 
    -- Declare account objects.
 
    B_Account : Accounts.Bank_Account;
    S_Account : Accounts.S_And_L.Savings_Account;
    P_Account : Accounts.S_And_L.Premium.Preferred_Account;
 
    -- Procedures to operate on accounts.
    -- Each uses a class-wide IN parameter, as well as a call to a
    -- dispatching operation.
 
    -- Function Tabulate_Account performs a dispatching call on a primitive
    -- operation that has been overridden for each of the extended types.
 
    procedure Tabulate_Account (Acct : in Accounts.Bank_Account'Class) is
    begin
       Accounts.Increment_Counters (Acct);   -- Dispatch according to tag.
    end Tabulate_Account;
 
    -- Function Accumulate_Reserve performs a dispatching call on a
    -- primitive operation that has been defined for the root type and 
    -- inherited by each derived type.
 
    function Accumulate_Reserve (Acct : in Accounts.Bank_Account'Class) 
      return Dollar_Amount is
    begin
       -- Dispatch according to tag.
       return (Accounts.Increment_Bank_Reserve (Acct));   
    end Accumulate_Reserve;
 
    -- Procedure Resolve_Dispute performs a dispatching call on a primitive
    -- operation that has been defined in the root type, overridden in the
    -- first derived extended type, and inherited by the subsequent extended
    -- type.
 
    procedure Resolve_Dispute (Acct : in Accounts.Bank_Account'Class) is
    begin
       -- Dispatch according to tag.
       Daily_Representative := Accounts.Assign_Representative (Acct);   
    end Resolve_Dispute;
 
      --=================================================================--
 
 begin  -- Main test procedure.
 
    Report.Test ("C392003", "Check that the use of a class-wide parameter "   &
                             "allows for proper dispatching where root type " &
                             "is declared in a nested package, and "          &
                             "subsequent extended types are derived in "      &
                             "further nested packages" );
    
    Bank_Account_Subtest:
    begin
       Accounts.Open (B_Account);
 
       -- Demonstrate class-wide parameter allowing dispatch by a primitive
       -- operation that has been defined for this specific type.
       Bank_Reserve := Accumulate_Reserve (Acct => B_Account);
       Tabulate_Account (B_Account);
 
       if (Bank_Reserve /= Opening_Balance) or
          (Number_Of_Accounts (Bank) /= 1)  or
          (Number_Of_Accounts (Total) /= 1) 
       then
          Report.Failed ("Failed in Bank_Account_Subtest");
       end if;
 
    end Bank_Account_Subtest;
 
 
    Savings_Account_Subtest:
    begin
       Accounts.S_And_L.Open (Acct => S_Account);
 
       -- Demonstrate class-wide parameter allowing dispatch by a primitive
       -- operation that has been overridden for this extended type.
       Resolve_Dispute  (Acct => S_Account);
       Tabulate_Account (S_Account);
 
       if (Daily_Representative /= Manager)   or 
          (Number_Of_Accounts (Savings) /= 1) or
          (Number_Of_Accounts (Total) /= 2)
       then
          Report.Failed ("Failed in Savings_Account_Subtest");
       end if;
 
    end Savings_Account_Subtest;
 
 
 
    Preferred_Account_Subtest:
    begin
       Accounts.S_And_L.Premium.Open (P_Account);
 
       -- Verify that the correct implementation of Open (overridden) was 
       -- used for the Preferred_Account object.
       if not Accounts.S_And_L.Premium.Verify_Open (P_Account) then
          Report.Failed ("Incorrect values for init. Preferred Acct object");
       end if;
 
       -- Demonstrate class-wide parameter allowing dispatch by a primitive
       -- operation that has been twice inherited by this extended type.
       Bank_Reserve := Accumulate_Reserve (Acct => P_Account);
 
       -- Demonstrate class-wide parameter allowing dispatch by a primitive
       -- operation that has been overridden for this extended type (the
       -- operation was overridden by its parent type as well).
       Tabulate_Account (P_Account);
       
       if Bank_Reserve /= 1100.00             or 
          Number_Of_Accounts (Preferred) /= 1 or
          Number_Of_Accounts (Total) /= 3
       then
          Report.Failed ("Failed in Preferred_Account_Subtest");
       end if;
 
    end Preferred_Account_Subtest;
 
    Report.Result;
 
 end C392003;
