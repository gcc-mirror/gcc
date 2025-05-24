-- CB40A021.AM
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
--      Check that a user defined exception is correctly propagated from a
--      private child subprogram to its parent and then to a client of the
--      parent.
--
-- TEST DESCRIPTION:
--      Declare a child package containing a function.  The body of the 
--      function contains a call to a private child subprogram (child of
--      the child).  The private child subprogram raises an exception
--      defined in the root ancestor package, and it is propagated to the
--      test program.
--
--      Exception Type Raised:
--        * User Defined
--          Predefined  
--
--      Hierarchical Structure Employed For This Test:  
--        * Parent Package
--        * Visible Child Package
--          Private Child Package
--          Visible Child Subprogram
--        * Private Child Subprogram
--
-- TEST FILES:
--      This test consists of the following files:
--
--         FB40A00.A
--         CB40A020.A
--      => CB40A021.AM
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      02 Nov 96   SAIC    ACVC 2.1: Modified prologue.
--
--!


with Report;
with FB40A00.CB40A020_0;   -- Explicit "with" of Text_Parser.Processing
                           -- Implicit "with" of Text_Parser (FB40A00)

procedure CB40A021 is

   String_Constant : constant String := 
     "ACVC Version 2.0 will incorporate Ada 9X feature tests."; 

   Number_Of_AlphaNumeric_Characters : Natural := 0;

begin

   Process_Block:
   begin

      Report.Test ("CB40A021", "Check that a user defined exception " &
                               "is correctly propagated across "      &
                               "package and subprogram boundaries");

      Number_Of_AlphaNumeric_Characters := 
        FB40A00.CB40A020_0.Count_AlphaNumerics (String_Constant);

      Report.Failed ("Exception should have been handled");

   exception

      when FB40A00.Completed_Text_Processing =>      -- Correct exception 
         if FB40A00.AlphaNumeric_Count /= 45 then    -- propagation.
            Report.Failed ("Incorrect string processing");
         end if;

      when others =>
         Report.Failed ("Exception handled in an others handler");

   end Process_Block;

   Report.Result;

end CB40A021;
