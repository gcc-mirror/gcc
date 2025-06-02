-- C47002C.ADA

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
-- CHECK THAT VALUES BELONGING TO EACH CLASS OF TYPE CAN BE WRITTEN AS 
-- THE OPERANDS OF QUALIFIED EXPRESSIONS.
-- THIS TEST IS FOR ARRAY, RECORD, AND ACCESS TYPES.

-- RJW 7/23/86

WITH REPORT; USE REPORT; 
PROCEDURE C47002C IS

BEGIN

     TEST( "C47002C", "CHECK THAT VALUES HAVING ARRAY, RECORD, AND " &
                      "ACCESS TYPES CAN BE WRITTEN AS THE OPERANDS " &
                      "OF QUALIFIED EXPRESSIONS" );

     DECLARE -- ARRAY TYPES.
     
          TYPE ARR IS ARRAY (POSITIVE RANGE <>) OF INTEGER;
          SUBTYPE ARR1 IS ARR (1 .. 1);
          SUBTYPE ARR5 IS ARR (1 .. 5);

          TYPE NARR IS NEW ARR;
          SUBTYPE NARR2 IS NARR (2 .. 2);

          TYPE TARR IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) 
               OF INTEGER;
          SUBTYPE TARR15 IS TARR (1 .. 1, 1 .. 5);
          SUBTYPE TARR51 IS TARR (1 .. 5, 1 .. 1);

          TYPE NTARR IS NEW TARR;
          SUBTYPE NTARR26 IS NTARR (2 .. 6, 2 .. 6);

          FUNCTION F (X : ARR) RETURN ARR IS
          BEGIN
               RETURN X;
          END;

          FUNCTION F (X : NARR) RETURN NARR IS
          BEGIN
               RETURN X;
          END;

          FUNCTION F (X : TARR) RETURN TARR IS
          BEGIN
               RETURN X;
          END;

          FUNCTION F (X : NTARR) RETURN NTARR IS
          BEGIN
               RETURN X;
          END;

     BEGIN
          IF F (ARR1'(OTHERS => 0))'LAST /= 1 THEN
               FAILED ( "INCORRECT RESULTS FOR SUBTYPE ARR1" );
          END IF;

          IF F (ARR5'(OTHERS => 0))'LAST /= 5 THEN 
               FAILED ( "INCORRECT RESULTS FOR SUBTYPE ARR5" );
          END IF;

          IF F (NARR2'(OTHERS => 0))'FIRST /= 2 OR 
             F (NARR2'(OTHERS => 0))'LAST /= 2 THEN
               FAILED ( "INCORRECT RESULTS FOR SUBTYPE NARR2" );
          END IF;

          IF F (TARR15'(OTHERS => (OTHERS => 0)))'LAST /= 1 OR 
             F (TARR15'(OTHERS => (OTHERS => 0)))'LAST (2) /= 5 THEN
               FAILED ( "INCORRECT RESULTS FOR SUBTYPE TARR15" );
          END IF;

          IF F (TARR51'(OTHERS => (OTHERS => 0)))'LAST /= 5 OR 
             F (TARR51'(OTHERS => (OTHERS => 0)))'LAST (2) /= 1 THEN
               FAILED ( "INCORRECT RESULTS FOR SUBTYPE TARR51" );
          END IF;

          IF F (NTARR26'(OTHERS => (OTHERS => 0)))'FIRST /= 2 OR 
             F (NTARR26'(OTHERS => (OTHERS => 0)))'LAST /= 6 OR
             F (NTARR26'(OTHERS => (OTHERS => 0)))'FIRST (2) /= 2 OR 
             F (NTARR26'(OTHERS => (OTHERS => 0)))'LAST (2) /= 6 THEN
               FAILED ( "INCORRECT RESULTS FOR SUBTYPE NTARR26" );
          END IF;

     END;   
     
     DECLARE -- RECORD TYPES.

          TYPE GENDER IS (MALE, FEMALE, NEUTER);
          
          TYPE MAN IS
               RECORD
                    AGE : POSITIVE;
               END RECORD;

          TYPE WOMAN IS
               RECORD
                    AGE : POSITIVE;
               END RECORD;

          TYPE ANDROID IS NEW MAN;
          
          FUNCTION F (X: WOMAN) RETURN GENDER IS
          BEGIN
               RETURN FEMALE;
          END F;

          FUNCTION F (X: MAN) RETURN GENDER IS
          BEGIN
               RETURN MALE;
          END F;
          
          FUNCTION F (X : ANDROID) RETURN GENDER IS
          BEGIN
               RETURN NEUTER;
          END F;

     BEGIN
          IF F (MAN'(AGE => 23)) /= MALE THEN
               FAILED ( "INCORRECT RESULTS FOR SUBTYPE MAN" );
          END IF;

          IF F (WOMAN'(AGE => 38)) /= FEMALE THEN
               FAILED ( "INCORRECT RESULTS FOR SUBTYPE WOMAN" );
          END IF;

          IF F (ANDROID'(AGE => 2001)) /= NEUTER THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE ANDRIOD" );
          END IF;
     END;     
                    
     DECLARE -- ACCESS TYPES.

          TYPE CODE IS (OLD, BRANDNEW, WRECK);
          
          TYPE CAR (D : CODE) IS
               RECORD
                    NULL;
               END RECORD;

          TYPE KEY IS ACCESS CAR;

          TYPE KEY_OLD IS ACCESS CAR (OLD);
          KO : KEY_OLD := NEW CAR'(D => OLD);          

          TYPE KEY_WRECK IS ACCESS CAR (WRECK);

          TYPE KEY_CARD IS NEW KEY;
          KC : KEY_CARD := NEW CAR'(D => BRANDNEW);          
 
          FUNCTION F (X : KEY_OLD) RETURN CODE IS
          BEGIN
               RETURN OLD;
          END F;

          FUNCTION F (X : KEY_WRECK) RETURN CODE IS
          BEGIN
               RETURN WRECK;
          END F;

          FUNCTION F (X : KEY_CARD) RETURN CODE IS
          BEGIN
               RETURN BRANDNEW;
          END F;
     BEGIN
          IF KEY_OLD'(KO) /= KO THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE KEY_OLD - 1" );
          END IF;

          IF KEY_CARD'(KC) /= KC THEN 
               FAILED ( "INCORRECT RESULTS FOR TYPE KEY_CARD - 1" );
          END IF;


          IF F (KEY_OLD'(NULL)) /= OLD THEN
               FAILED ( "INCORRECT RESULTS FOR SUBTYPE KEY_OLD - 2" );
          END IF;

          IF F (KEY_WRECK'(NULL)) /= WRECK THEN
               FAILED ( "INCORRECT RESULTS FOR SUBTYPE KEY_WRECK" );
          END IF;

          IF F (KEY_CARD'(NULL)) /= BRANDNEW THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE KEY_CARD - 2" );
          END IF;
     END;
          
     RESULT;
END C47002C;
