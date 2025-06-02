-- CC3015A.ADA

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
-- CHECK THAT WHEN A GENERIC PACKAGE INSTANTIATION IS ELABORATED, 
-- STATEMENTS IN ITS PACKAGE BODY ARE EXECUTED AND EXPRESSIONS
-- REQUIRING EVALUATION ARE EVALUATED (E.G., DEFAULTS FOR OBJECT
-- DECLARATIONS ARE EVALUATED).

-- RJW 6/11/86

WITH REPORT; USE REPORT;

PROCEDURE CC3015A IS
     BOOL1, BOOL2 : BOOLEAN := FALSE;          
          
     TYPE ENUM IS (BEFORE, AFTER);

     FUNCTION F (I : INTEGER) RETURN INTEGER IS
     BEGIN
          BOOL2 := TRUE;
          RETURN I;
     END;
          
     FUNCTION CHECK (E : ENUM) RETURN CHARACTER IS
     BEGIN
          IF E = BEFORE THEN
               IF BOOL1 THEN
                    FAILED ( "STATEMENT EXECUTED BEFORE " &
                             "INSTANTIATION" );
               END IF;
               IF BOOL2 THEN
                    FAILED ( "DEFAULT EXPRESSION EVALUATED " &
                             "BEFORE INSTANTIATION" );
               END IF;
          ELSE
               IF BOOL1 THEN
                    NULL;
               ELSE
                    FAILED ( "STATEMENT NOT EXECUTED AT " &
                             "INSTANTIATION" );
               END IF;
               IF BOOL2 THEN
                    NULL;
               ELSE
                    FAILED ( "DEFAULT EXPRESSION NOT EVALUATED " &
                             "AT INSTANTIATION" );
               END IF;
          END IF;
          RETURN 'A';
      END;

     GENERIC 
          TYPE INT IS RANGE <>;
     PACKAGE PKG IS END PKG;

     PACKAGE BODY PKG IS
          I : INT := INT'VAL (F(0));
     BEGIN
          BOOL1 := TRUE;
     END;
                         
BEGIN
     TEST ("CC3015A", "CHECK THAT WHEN A GENERIC PACKAGE " & 
                      "INSTANTIATION IS ELABORATED, STATEMENTS " & 
                      "IN ITS PACKAGE BODY ARE EXECUTED AND " &
                      "EXPRESSIONS REQUIRING EVALUATION ARE " &
                      "EVALUATED (E.G., DEFAULTS FOR OBJECT " &
                      "DECLARATIONS ARE EVALUATED)" );
     

     DECLARE
          A : CHARACTER := CHECK (BEFORE);

          PACKAGE NPKG IS NEW PKG (INTEGER);
     
          B : CHARACTER := CHECK (AFTER);

     BEGIN
          NULL;
     END;          

     RESULT;
END CC3015A;
