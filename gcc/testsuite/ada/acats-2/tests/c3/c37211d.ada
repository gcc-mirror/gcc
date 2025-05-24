-- C37211D.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED BY A DISCRIMINANT CONSTRAINT 
-- IF A VALUE SPECIFIED FOR A DISCRIMINANT DOES NOT LIE IN THE RANGE
-- OF THE DISCRIMINANT. THIS TEST CONTAINS CHECKS FOR SUBTYPE 
-- INDICATIONS WHERE THE TYPE MARK DENOTES AN INCOMPLETE TYPE.

-- R.WILLIAMS 8/28/86
-- EDS        7/14/98    AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C37211D IS

     GLOBAL : BOOLEAN;

     TYPE DAY IS (SUN, MON, TUE, WED, THU, FRI, SAT);

     SUBTYPE WEEKDAY IS DAY RANGE MON .. FRI;

     FUNCTION SWITCH (B : BOOLEAN) RETURN BOOLEAN IS
     BEGIN
          GLOBAL := B;
          RETURN B;
     END SWITCH;
     
     FUNCTION IDENT (D : DAY) RETURN DAY IS
     BEGIN
          RETURN DAY'VAL (IDENT_INT (DAY'POS (D)));
     END IDENT;

BEGIN
     TEST ( "C37211D", "CHECK THAT CONSTRAINT_ERROR IS RAISED BY " &
                       "A DISCRIMINANT CONSTRAINT IF A VALUE " &
                       "SPECIFIED FOR A DISCRIMINANT DOES NOT LIE " &
                       "IN THE RANGE OF THE DISCRIMINANT WHERE THE " &
                       "TYPE MARK DENOTES AN INCOMPLETE TYPE" );
                         
     BEGIN
          DECLARE
                                   
               B1 : BOOLEAN := SWITCH (TRUE);

               TYPE REC (D : WEEKDAY);

               TYPE ACCREC IS ACCESS REC (IDENT (SUN));

               B2 : BOOLEAN := SWITCH (FALSE);

               TYPE REC (D : WEEKDAY) IS
                    RECORD
                         NULL;
                    END RECORD;
          BEGIN
               DECLARE
                    AC : ACCREC;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE ACCREC " & DAY'IMAGE(AC.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT AC" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF GLOBAL THEN 
                    NULL;
               ELSE
                    FAILED ( "EXCEPTION RAISED AT ELABORATION OF " &
                             "FULL TYPE REC NOT TYPE ACCREC" );
               END IF;                    
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE ACCREC" );
     END;

     RESULT;
END C37211D;
