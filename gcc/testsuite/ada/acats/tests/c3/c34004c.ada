-- C34004C.ADA

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
--     FOR DERIVED FIXED POINT TYPES:

--     CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR
--     THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--     CONSTRAINED.

--     CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--     IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 09/08/86
--     JLH 09/25/87  REFORMATTED HEADER.
--     JRL 03/13/92  MODIFIED TO DEFEAT OPTIMIZATION WHEN ATTEMPTING TO
--                   RAISE CONSTRAINT_ERROR.
--     JRL 03/30/93  REMOVED NUMERIC_ERROR FROM TEST.
--     DTN 11/30/95  REMOVED NON ADA95 ATTRIBUTES.               

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34004C IS

     TYPE PARENT IS DELTA 0.01 RANGE -100.0 .. 100.0;

     TYPE T IS NEW PARENT DELTA 0.1 RANGE
               IDENT_INT (1) * (-30.0) ..
               IDENT_INT (1) * ( 30.0);

     SUBTYPE SUBPARENT IS PARENT DELTA 0.1 RANGE -30.0 .. 30.0;

     TYPE S IS NEW SUBPARENT;

     X,XA : T;
     Y,YA : S;


     FUNCTION OUT_OF_BOUNDS ( VAR1 , VAR2 : T ) RETURN BOOLEAN IS
     BEGIN
        IF ( VAR1 + VAR2 ) IN T THEN
           RETURN FALSE ;
        ELSE
           RETURN TRUE ;
        END IF ;
     EXCEPTION
        WHEN CONSTRAINT_ERROR =>
           RETURN TRUE ;
     END OUT_OF_BOUNDS ;


     FUNCTION OUT_OF_BOUNDS ( VAR1 , VAR2 : S ) RETURN BOOLEAN IS
     BEGIN
        IF ( VAR1 + VAR2 ) IN S THEN
           RETURN FALSE ;
        ELSE
           RETURN TRUE ;
        END IF ;
     EXCEPTION
        WHEN CONSTRAINT_ERROR =>
           RETURN TRUE ;
     END OUT_OF_BOUNDS ;


BEGIN
     TEST ("C34004C", "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
                      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
                      "WHEN THE DERIVED TYPE DEFINITION IS " &
                      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
                      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
                      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
                      "FIXED POINT TYPES");

     -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

     DECLARE
          TBD : CONSTANT := BOOLEAN'POS (T'BASE'DELTA <= 0.01);
          SBD : CONSTANT := BOOLEAN'POS (S'BASE'DELTA <= 0.01);
     BEGIN
          IF TBD = 0 OR SBD = 0 THEN
               FAILED ("INCORRECT 'BASE'DELTA");
          END IF;
     END;


     DECLARE
          N : INTEGER := IDENT_INT (8);
     BEGIN
          IF  98.0 + T'(1.0) + N * 0.0078125 /=  99.0625 OR
              98.0 + S'(1.0) + 8 * 0.0078125 /=  99.0625 OR
             -98.0 - T'(1.0) - N * 0.0078125 /= -99.0625 OR
             -98.0 - S'(1.0) - 8 * 0.0078125 /= -99.0625 THEN
               FAILED ("INCORRECT + OR -");
          END IF;
     END;


     IF T'FIRST /= -30.0 OR T'LAST /= 30.0 OR
        S'FIRST /= -30.0 OR S'LAST /= 30.0 THEN
          FAILED ("INCORRECT 'FIRST OR 'LAST");
     END IF;

     BEGIN
          X := -30.0;
          Y := -30.0;
          IF PARENT (X) /= PARENT (Y) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT - 1");
          END IF;
          X := 30.0;
          Y := 30.0;
          IF PARENT (X) /= PARENT (Y) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT - 2");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY OK ASSIGNMENT");
     END;


     BEGIN
          X  := -30.0 ;
          XA := -0.0625 ;
          IF NOT OUT_OF_BOUNDS ( X , XA ) THEN
             FAILED ( "CONSTRAINT_ERROR NOT RAISED -- X := -30.0625" ) ;
          END IF ;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- X := -30.0625");
     END;


     BEGIN
          X  := 30.0 ;
          XA := 0.0625 ;
          IF NOT OUT_OF_BOUNDS ( X , XA ) THEN
             FAILED ( "CONSTRAINT_ERROR NOT RAISED -- X := 30.0625" ) ;
          END IF ;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- X := 30.0625");
     END;


     BEGIN
          Y  := -30.0 ;
          YA := -0.0625 ;
          IF NOT OUT_OF_BOUNDS ( Y , YA ) THEN
             FAILED ( "CONSTRAINT_ERROR NOT RAISED -- Y := -30.0625" ) ;
          END IF ;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- Y := -30.0625");
     END;


     BEGIN
          Y  := 30.0 ;
          YA := 0.0625 ;
          IF NOT OUT_OF_BOUNDS ( Y , YA ) THEN
             FAILED ( "CONSTRAINT_ERROR NOT RAISED -- Y := 30.0625" ) ;
          END IF ;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- Y := 30.0625");
     END;

     RESULT;
END C34004C;
