-- CDA201E.ADA

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
-- OBJECTIVE:
--     CHECK THAT UNCHECKED_CONVERSION CAN BE INSTANTIATED FOR THE
--     CONVERSION OF AN ENUMERATION TYPE WITH A REPRESENTATION CLAUSE TO
--     INTEGER.

-- HISTORY:
--     JET 09/23/88  CREATED ORIGINAL TEST.
--     DHH 05/17/89  CHANGED FROM '.DEP' TEST TO '.ADA' TEST.
--     RJW 02/28/90  ADDED SIZE CLAUSE FOR TYPE STOOGE.
--     LDC 09/20/90  ADDED CHECK FOR CONVERSION FROM INT TO STOOGE,
--                   ADDED COMMENT WHEN SIZES AREN'T EQUAL.

WITH REPORT; USE REPORT;
WITH UNCHECKED_CONVERSION;
PROCEDURE CDA201E IS

     TYPE STOOGE IS (CURLY, MOE, LARRY);
     FOR STOOGE USE (CURLY => -5, MOE => 13, LARRY => 127);
     FOR STOOGE'SIZE USE 8;

     TYPE INT IS RANGE -128 .. 127;
     FOR INT'SIZE USE 8;

     I    : INT := 0;
     NAME : STOOGE := CURLY;

     FUNCTION E_TO_I IS NEW UNCHECKED_CONVERSION(STOOGE, INT);
     FUNCTION I_TO_E IS NEW UNCHECKED_CONVERSION(INT, STOOGE);

     FUNCTION ID(E : STOOGE) RETURN STOOGE IS
     BEGIN
          RETURN STOOGE'VAL(STOOGE'POS(E) + IDENT_INT(0));
     END ID;

     FUNCTION ID_INT (X : INT) RETURN INT IS
          A : INTEGER := IDENT_INT(3);
     BEGIN
          IF EQUAL (A, IDENT_INT(3)) THEN    -- ALWAYS EQUAL.
               RETURN X;                     -- ALWAYS EXECUTED.
          END IF;
          RETURN 0;                          -- NEVER EXECUTED.
     END ID_INT;

BEGIN
     TEST ("CDA201E", "CHECK THAT UNCHECKED_CONVERSION CAN BE " &
                      "INSTANTIATED FOR THE CONVERSION OF AN " &
                      "ENUMERATION TYPE WITH A REPRESENTATION " &
                      "CLAUSE TO INTEGER");

     IF I'SIZE /= NAME'SIZE THEN
          COMMENT( "UNCHECKED_CONVERSION MIGHT BE INSTANTIATED WITH " &
                   "DIFFERNT SIZES");
     END IF;

     BEGIN
          I := E_TO_I(ID(CURLY));
          IF I /= -5 THEN
               FAILED ("INCORRECT VALUE OF CURLY: " & INT'IMAGE(I));
          END IF;

          I := E_TO_I(ID(MOE));
          IF I /= 13 THEN
               FAILED ("INCORRECT VALUE OF MOE: " & INT'IMAGE(I));
          END IF;

          I := E_TO_I(ID(LARRY));
          IF I /= 127 THEN
               FAILED ("INCORRECT VALUE OF LARRY: " & INT'IMAGE(I));
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED BY CONVERSION");
     END;

     BEGIN     -- 2
          NAME := I_TO_E(ID_INT(-5));
          IF NAME /= CURLY THEN
               FAILED ("INCORRECT VALUE OF -5 : " & STOOGE'IMAGE(NAME));
          END IF;

          NAME := I_TO_E(ID_INT(13));
          IF NAME /= MOE THEN
               FAILED ("INCORRECT VALUE OF 13: " & STOOGE'IMAGE(NAME));
          END IF;

          NAME := I_TO_E(ID_INT(127));
          IF NAME /= LARRY THEN
               FAILED ("INCORRECT VALUE OF 127: " & STOOGE'IMAGE(NAME));
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED BY CONVERSION - 2");
     END;

     RESULT;
END CDA201E;
