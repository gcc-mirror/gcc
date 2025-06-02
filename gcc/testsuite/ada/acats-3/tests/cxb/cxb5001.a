-- CXB5001.A
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
--      Check that the specification of the package Interfaces.Fortran
--      are available for use.
--
-- TEST DESCRIPTION:
--      This test verifies that the types and subprograms specified for the
--      interface are present
--
-- APPLICABILITY CRITERIA: 
--      If an implementation provides package Interfaces.Fortran, this test
--      must compile, execute, and report "PASSED".
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      28 Feb 96   SAIC    Added applicability criteria.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Report;
with Interfaces.Fortran;                                        -- N/A => ERROR

procedure CXB5001 is
   package Fortran renames Interfaces.FORTRAN;

begin

   Report.Test ("CXB5001", "Check the specification of Interfaces.Fortran");


   declare  -- encapsulate the test


         TC_Int     : integer := 1;
         TC_Natural : natural;
         TC_String  : String := "ABCD";
         TC_Character : Character := 'a';

         TST_Fortran_Integer   :  FORTRAN.Fortran_Integer;

         TST_Real              : Fortran.Real;
         TST_Double_Precision  : Fortran.Double_Precision;

         TST_Logical : Fortran.Logical := FORTRAN.true;     
                                                   -- verify it is a Boolean
         TST_Complex : Fortran.Complex;

         TST_Imaginary_i : Fortran.Imaginary := FORTRAN.i;
         TST_Imaginary_j : Fortran.Imaginary := FORTRAN.j;


         -- Initialize it so we can use it below
         TST_Character_Set : Fortran.Character_Set :=   
                                                Fortran.Character_Set'First;

         TST_Fortran_Character : FORTRAN.Fortran_Character (1..5) := 
                                       (others => TST_Character_Set);



   begin    -- encapsulation

      -- Arrange that the calls to the subprograms are compiled but
      -- not executed
      -- 
      if not Report.Equal ( TC_Int, TC_Int ) then

         TST_Character_Set := Fortran.To_Fortran (TC_Character);
         TC_Character := Fortran.To_Ada (TST_Character_Set);
      

         TST_Fortran_Character := FORTRAN.To_Fortran ("TEST STRING");   
         Report.Comment ( Fortran.To_Ada (TST_Fortran_Character) );

         Fortran.To_Fortran ( TC_String, TST_Fortran_Character, TC_Natural );
         Fortran.To_Ada ( TST_Fortran_Character, TC_String, TC_Natural );
         
      end if;

   end;     -- encapsulation

   Report.Result;

end CXB5001;
