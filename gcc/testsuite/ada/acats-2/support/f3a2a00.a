-- F3A2A00.A
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
-- FOUNDATION DESCRIPTION:
--      This foundation declares support types and subprograms for testing
--      run-time accessibility checks.
--
-- CHANGE HISTORY:
--      01 May 95   SAIC    Initial prerelease version.
--
--!

package F3A2A00 is

   type Tagged_Type is tagged record
      C: Integer := 0;
   end record;

   type Array_Type is array (1 .. 10) of Tagged_Type;

   type AccTag_L0      is access all Tagged_Type;
   type AccTagClass_L0 is access all Tagged_Type'Class;

   type AccArr_L0 is access all Array_Type;

   X_L0 : Tagged_Type;


   type TC_Result_Kind is (OK, P_E, O_E);

   procedure TC_Display_Results (Actual  : in TC_Result_Kind;
                                 Expected: in TC_Result_Kind;
                                 Message : in String);
end F3A2A00;


     --==================================================================--


with Report;
package body F3A2A00 is

   procedure TC_Display_Results (Actual  : in TC_Result_Kind;
                                 Expected: in TC_Result_Kind;
                                 Message : in String) is
   begin
      if Actual /= Expected then
         case Actual is
            when OK  =>
               Report.Failed ("No exception raised: "         & Message);
            when P_E =>
               Report.Failed ("Program_Error raised: "        & Message);
            when O_E =>
               Report.Failed ("Unexpected exception raised: " & Message);
         end case;
      end if;
   end TC_Display_Results;

end F3A2A00;
