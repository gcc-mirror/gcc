-- IMPDEFH.A
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
-- DESCRIPTION:
--      This package is used to define those values that are implementation
--      defined for use with validating the Safety and Security special needs
--      annex, Annex-H.
--
-- APPLICABILITY CRITERIA:
--     This package is only required for implementations validating the
--     Safety and Security Annex.
--
-- CHANGE HISTORY:
--      13 FEB 96   SAIC    Initial version
--      25 NOV 96   SAIC    Revised for release 2.1
--
--!

package Impdef.Annex_H is

  type Scalar_To_Normalize is
       (   Id0,  Id1,  Id2,  Id3,  Id4,  Id5,  Id6,  Id7,  Id8,  Id9,
          Id10, Id11, Id12, Id13, Id14, Id15, Id16, Id17, Id18, Id19,
          Id20, Id21, Id22, Id23, Id24, Id25, Id26, Id27, Id28, Id29,
          Id30, Id31, Id32, Id33, Id34, Id35, Id36, Id37, Id38, Id39,
          Id40, Id41, Id42, Id43, Id44, Id45, Id46, Id47, Id48, Id49,
          Id50, Id51, Id52, Id53, Id54, Id55, Id56, Id57, Id58, Id59,
          Id60, Id61, Id62, Id63, Id64, Id65, Id66, Id67, Id68, Id69,
          Id70, Id71, Id72, Id73, Id74, Id75, Id76, Id77, Id78, Id79,
          Id80, Id81, Id82, Id83, Id84, Id85, Id86, Id87, Id88, Id89,
          Id90, Id91, Id92, Id93, Id94, Id95, Id96, Id97, Id98, Id99,
          IdA0, IdA1, IdA2, IdA3, IdA4, IdA5, IdA6, IdA7, IdA8, IdA9,
          IdB0, IdB1, IdB2, IdB3, IdB4, IdB5, IdB6 );

  -- NO MODIFICATION NEEDED TO TYPE SCALAR_TO_NORMALIZE.  DO NOT MODIFY.

  type Small_Number is range 1..100;

  -- NO MODIFICATION NEEDED TO TYPE SMALL_NUMBER.  DO NOT MODIFY.

--=====================================================================
  -- When the value documented in H.1(5) as the predictable initial value
  -- for an uninitialized object of the type Scalar_To_Normalize
  -- (an enumeration type containing 127 identifiers) is to be in the range
  -- Id0..IdB6, set the following constant to True; otherwise leave it set
  -- to False.

  Default_For_Scalar_To_Normalize_Is_In_Range : constant Boolean := False;
  --                                      MODIFY HERE AS NEEDED --- ^^^^^

--=====================================================================
  -- If the above constant Default_For_Scalar_To_Normalize_Is_In_Range is
  -- set True, the following constant must be set to the value documented
  -- in H.1(5) as the predictable initial value for the type
  -- Scalar_To_Normalize.

  Default_For_Scalar_To_Normalize : constant Scalar_To_Normalize := Id0;
  --                                      MODIFY HERE AS NEEDED --- ^^^

--=====================================================================
  -- When the value documented in H.1(5) as the predictable initial value
  -- for an uninitialized object of the type Small_Number
  -- (an integer type containing 100 values) is to be in the range
  -- 1..100, set the following constant to True; otherwise leave it set
  -- to False.

  Default_For_Small_Number_Is_In_Range : constant Boolean := False;
  --                               MODIFY HERE AS NEEDED --- ^^^^^

--=====================================================================
  -- If the above constant Default_For_Small_Number_Is_In_Range is
  -- set True, the following constant must be set to the value documented
  -- in H.1(5) as the predictable initial value for the type Small_Number.

  Default_For_Small_Number : constant Small_Number := 100;
  --                        MODIFY HERE AS NEEDED --- ^^^

--=====================================================================

end Impdef.Annex_H;
