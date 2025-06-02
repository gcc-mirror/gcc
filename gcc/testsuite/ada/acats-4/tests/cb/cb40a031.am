-- CB40A031.AM
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
--      Check that a predefined exception is correctly propagated from
--      a private child package through a visible child package to a client.
--
-- TEST DESCRIPTION:
--      Declare two child packages from a root package, one visible, one
--      private.  The visible child package contains a function, whose
--      body makes a call to a procedure contained in the private sibling
--      package.  A predefined exception occurring in the subprogram within the
--      private package is propagated through the visible sibling and ancestor
--      to the test program.
--
--      Exception Type Raised:
--          User Defined
--        * Predefined  
--
--      Hierarchical Structure Employed For This Test:  
--        * Parent Package
--        * Visible Child Package
--        * Private Child Package
--          Visible Child Subprogram
--          Private Child Subprogram
--
-- TEST FILES:
--      This test consists of the following files:
--
--         FB40A00.A
--         CB40A030.A
--      => CB40A031.AM
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      02 Nov 96   SAIC    ACVC 2.1: Modified prologue.
--
--!

with Report;
with FB40A00.CB40A030_0; -- Explicit "with" of Text_Parser.Character_Counting
                         -- Implicit "with" of Text_Parser

procedure CB40A031 is

   String_Constant : constant String := 
     "The San Diego Padres will win the World Series in 1999."; 

   Number_Of_AlphaNumeric_Characters : Natural := 0;

begin

   Process_Block:
   begin

      Report.Test ("CB40A031", "Check that a predefined exception " &
                               "is correctly propagated across "    &
                               "package boundaries");

      Number_Of_AlphaNumeric_Characters := 
        FB40A00.CB40A030_0.Count_AlphaNumerics (String_Constant);

      Report.Failed ("Exception should have been handled");

   exception

      when Constraint_Error =>                       -- Correct exception 
         if FB40A00.AlphaNumeric_Count /= 44 then    -- propagation.
            Report.Failed ("Incorrect string processing");
         end if;

      when others =>
         Report.Failed ("Exception handled in an others handler");

   end Process_Block;

   Report.Result;

end CB40A031;
