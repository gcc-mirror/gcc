-- C87B07B.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- FOR THE ATTRIBUTE OF THE FORM T'VAL (X), THE OPERAND X MAY
-- BE OF ANY INTEGER TYPE. THE RESULT IS OF TYPE T.
  
-- TRH  15 SEPT 82
-- DSJ  06 JUNE 83
-- PWN  01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B07B IS
 
     TYPE NEW_INT IS NEW INTEGER;
     TYPE WHOLE   IS NEW INTEGER RANGE 0 .. INTEGER'LAST;
     TYPE FLAG IS (PASS, FAIL);
     
     FUNCTION "+" (X, Y : NEW_INT) RETURN NEW_INT
          RENAMES "-";
     FUNCTION "+" (X, Y : WHOLE)   RETURN WHOLE
          RENAMES "*";
 
     GENERIC
          TYPE T IS PRIVATE;
          ARG  : IN T;
          STAT : IN FLAG;
     FUNCTION F1 RETURN T;
 
     FUNCTION F1 RETURN T IS
     BEGIN 
          IF STAT = FAIL THEN 
               FAILED ("THE 'VAL' ATTRIBUTE TAKES AN OPERAND " &
                       "OF AN INTEGER TYPE");
          END IF;
          RETURN ARG;
     END F1;
  
     FUNCTION F IS NEW F1 (CHARACTER, '1', FAIL);
     FUNCTION F IS NEW F1 (DURATION,  1.0, FAIL);
     FUNCTION F IS NEW F1 (FLOAT,     1.0, FAIL);
     FUNCTION F IS NEW F1 (NEW_INT,   1,   PASS);
    
BEGIN
     TEST ("C87B07B","OVERLOADED OPERANDS TO THE 'VAL' ATTRIBUTE");
    
     IF (INTEGER'VAL (F) /= 1)          THEN
          FAILED ("RESOLUTION INCORRECT - THE 'VAL' ATTRIBUTE " &
                  "MUST RETURN A VALUE OF TYPE T - 1");
     END IF;

     IF (INTEGER'VAL (3 + 3) + 1 /= 7)  THEN
          FAILED ("RESOLUTION INCORRECT - THE 'VAL' ATTRIBUTE " &
                  "MUST RETURN A VALUE OF TYPE T - 2");
     END IF;

     IF (NEW_INT'VAL (F) /= 1)          THEN
          FAILED ("RESOLUTION INCORRECT - THE 'VAL' ATTRIBUTE " &
                  "MUST RETURN A VALUE OF TYPE T - 3");
     END IF;

     IF (NEW_INT'VAL  (3 + 3) + 1 /= 5) THEN
          FAILED ("RESOLUTION INCORRECT - THE 'VAL' ATTRIBUTE " &
                  "MUST RETURN A VALUE OF TYPE T - 4");
     END IF;

     IF (WHOLE'VAL    (F) /= 1)         THEN
          FAILED ("RESOLUTION INCORRECT - THE 'VAL' ATTRIBUTE " &
                  "MUST RETURN A VALUE OF TYPE T - 5");
     END IF;

     IF (WHOLE'VAL    (3 + 3) + 1 /= 6) THEN
          FAILED ("RESOLUTION INCORRECT - THE 'VAL' ATTRIBUTE " &
                  "MUST RETURN A VALUE OF TYPE T - 6");
     END IF;
 
     RESULT;
END C87B07B;
