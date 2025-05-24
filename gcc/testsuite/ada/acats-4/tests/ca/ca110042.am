-- CA110042.AM
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
--      Check that the private part of a child library unit package can
--      utilize its parent unit's visible definitions.
--
-- TEST DESCRIPTION:
--      Declare a public library unit package and child package, with the
--      child package having a private part in the specification.  Within
--      this child private part, make use of components that are declared in
--      the visible part of the parent.
--
--      Demonstrate visibility to the following parent components in the 
--      child private part:
--                          Parent
--          Type              X
--          Constant          X
--          Object            X           
--          Subprogram        X           
--          Exception         X           
--
--
-- TEST FILES:
--      The following files comprise this test:
--
--         CA110040.A
--         CA110041.A
--      => CA110042.AM
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!
with Report;
with CA110040.CA110041;

procedure CA110042 is

      package System_Manager renames CA110040.CA110041;
      use CA110040;
      User1, User2, User3 : System_Manager.User_Account;

begin

   Report.Test ("CA110042", "Check that the private part of a child "     &
                            "library unit package can utilize its "       &
                            "parent unit's visible definitions");

   Assign_New_Accounts:            -- This code simulates the entering of new
                                   -- user accounts into a computer system.
                                   -- It also simulates the processing that
                                   -- could occur when the limit on system
                                   -- accounts has been exceeded.

                                   -- This processing block demonstrates the
                                   -- use of child package functionality that
                                   -- takes advantage of components declared in
                                   -- the parent package.
   begin

      if Total_Accounts /= 2 then
         Report.Failed ("Incorrect number of accounts currently allocated");
      end if;                                         -- At this point, both
                                                      -- System_Account and
                                                      -- Auditor_Account have
                                                      -- been declared and
                                                      -- initialized in package
                                                      -- CA110040.CA110041.

      System_Manager.Initialize_User_Account (User1); -- User_ID has been
                                                      -- set to 3.

      System_Manager.Initialize_User_Account (User2); -- User_ID has been
                                                      -- set to 4, which
                                                      -- is the last value
                                                      -- defined for the
                                                      -- CA110040.ID_Type
                                                      -- range.

      System_Manager.Initialize_User_Account (User3); -- This final call will 
                                                      -- result in an 
                                                      -- Account_Limit_Exceeded
                                                      -- exception being raised.

      Report.Failed ("Control should have transferred with exception");

   exception

      when Account_Limit_Exceeded =>
         if (not (Administrator_Account.User_ID = ID_Type'First)) or
           (User2.User_ID /= CA110040.ID_Type'Last)
         then
            Report.Failed ("Account initialization failure");
         end if;
      when others =>
         Report.Failed ("Unexpected exception raised");

   end Assign_New_Accounts;

   if (User1.User_ID /= 3) or (User2.User_ID /= 4) then
      Report.Failed ("Improper initialization of user accounts");
   end if;

   Report.Result;

end CA110042;
