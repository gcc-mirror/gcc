-- C47002D.ADA

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
-- THIS TEST IS FOR PRIVATE AND LIMITED PRIVATE TYPES.

-- RJW 7/23/86

WITH REPORT; USE REPORT; 
PROCEDURE C47002D IS

BEGIN

     TEST( "C47002D", "CHECK THAT VALUES HAVING PRIVATE AND LIMITED " &
                      "PRIVATE TYPES CAN BE WRITTEN AS THE OPERANDS " &
                      "OF QUALIFIED EXPRESSIONS" );

     DECLARE -- PRIVATE TYPES.

          TYPE RESULTS IS (P1, P2, P3, P4, P5);

          PACKAGE PKG1 IS
               TYPE PINT IS PRIVATE;
               TYPE PCHAR IS PRIVATE;
               TYPE PARR IS PRIVATE;
               TYPE PREC (D : INTEGER) IS PRIVATE;
               TYPE PACC IS PRIVATE;

               FUNCTION F RETURN PINT;
               FUNCTION F RETURN PCHAR;
               FUNCTION F RETURN PARR;
               FUNCTION F RETURN PREC;
               FUNCTION F RETURN PACC;

          PRIVATE
               TYPE PINT IS NEW INTEGER;
               TYPE PCHAR IS NEW CHARACTER;
               TYPE PARR IS ARRAY (1 .. 2) OF NATURAL;

               TYPE PREC (D : INTEGER) IS
                    RECORD
                         NULL;
                    END RECORD;
     
               TYPE PACC IS ACCESS PREC;

          END PKG1;
          
          PACKAGE BODY PKG1 IS
               FUNCTION F RETURN PINT IS
               BEGIN
                    RETURN 1;
               END F;
                    
               FUNCTION F RETURN PCHAR IS
               BEGIN
                    RETURN 'B';
               END F;

               FUNCTION F RETURN PARR IS
               BEGIN
                    RETURN PARR'(OTHERS => 3);
               END F;

               FUNCTION F RETURN PREC IS
               BEGIN
                    RETURN PREC'(D => 4);
               END F;

               FUNCTION F RETURN PACC IS
               BEGIN
                    RETURN NEW PREC'(F);
               END F;

          END PKG1;
                    
          PACKAGE PKG2 IS END PKG2;

          PACKAGE BODY PKG2 IS
               USE PKG1;

               FUNCTION CHECK (P : PINT) RETURN RESULTS IS
               BEGIN
                    RETURN  P1;
               END CHECK;
     
               FUNCTION CHECK (P : PCHAR) RETURN RESULTS IS
               BEGIN
                    RETURN  P2;
               END CHECK;
     
               FUNCTION CHECK (P : PARR) RETURN RESULTS IS
               BEGIN
                    RETURN  P3;
               END CHECK;
     
               FUNCTION CHECK (P : PREC) RETURN RESULTS IS
               BEGIN
                    RETURN  P4;
               END CHECK;

               FUNCTION CHECK (P : PACC) RETURN RESULTS IS
               BEGIN
                    RETURN  P5;
               END CHECK;
          
          BEGIN              
               IF CHECK (PINT'(F)) /= P1 THEN
                    FAILED ( "INCORRECT RESULTS FOR TYPE PINT" );
               END IF;

               IF CHECK (PCHAR'(F)) /= P2 THEN
                    FAILED ( "INCORRECT RESULTS FOR TYPE PCHAR" );
               END IF;
                 
               IF CHECK (PARR'(F)) /= P3 THEN
                    FAILED ( "INCORRECT RESULTS FOR TYPE PARR" );
               END IF;
                 
               IF CHECK (PREC'(F)) /= P4 THEN
                    FAILED ( "INCORRECT RESULTS FOR TYPE PREC" );
               END IF;

               IF CHECK (PACC'(F)) /= P5 THEN
                    FAILED ( "INCORRECT RESULTS FOR TYPE PACC" );
               END IF;
                 
          END PKG2;

     BEGIN
          NULL;
     END;

     DECLARE -- LIMITED PRIVATE TYPES.

          TYPE RESULTS IS (LP1, LP2, LP3, LP4, LP5);

          PACKAGE PKG1 IS
               TYPE LPINT IS LIMITED PRIVATE;
               TYPE LPCHAR IS LIMITED PRIVATE;
               TYPE LPARR IS LIMITED PRIVATE;
               TYPE LPREC (D : INTEGER) IS LIMITED PRIVATE;
               TYPE LPACC IS LIMITED PRIVATE;

               FUNCTION F RETURN LPINT;
               FUNCTION F RETURN LPCHAR;
               FUNCTION F RETURN LPARR;
               FUNCTION F RETURN LPREC;
               FUNCTION F RETURN LPACC;

          PRIVATE
               TYPE LPINT IS NEW INTEGER;
               TYPE LPCHAR IS NEW CHARACTER;
               TYPE LPARR IS ARRAY (1 .. 2) OF NATURAL;

               TYPE LPREC (D : INTEGER) IS
                    RECORD
                         NULL;
                    END RECORD;
     
               TYPE LPACC IS ACCESS LPREC;

          END PKG1;
          
          PACKAGE BODY PKG1 IS
               FUNCTION F RETURN LPINT IS
               BEGIN
                    RETURN 1;
               END F;
                    
               FUNCTION F RETURN LPCHAR IS
               BEGIN
                    RETURN 'B';
               END F;

               FUNCTION F RETURN LPARR IS
               BEGIN
                    RETURN LPARR'(OTHERS => 3);
               END F;

               FUNCTION F RETURN LPREC IS
               BEGIN
                    RETURN LPREC'(D => 4);
               END F;

               FUNCTION F RETURN LPACC IS
               BEGIN
                    RETURN NEW LPREC'(F);
               END F;

          END PKG1;
                    
          PACKAGE PKG2 IS END PKG2;

          PACKAGE BODY PKG2 IS
               USE PKG1;

               FUNCTION CHECK (LP : LPINT) RETURN RESULTS IS
               BEGIN
                    RETURN  LP1;
               END CHECK;
     
               FUNCTION CHECK (LP : LPCHAR) RETURN RESULTS IS
               BEGIN
                    RETURN  LP2;
               END CHECK;
     
               FUNCTION CHECK (LP : LPARR) RETURN RESULTS IS
               BEGIN
                    RETURN  LP3;
               END CHECK;
     
               FUNCTION CHECK (LP : LPREC) RETURN RESULTS IS
               BEGIN
                    RETURN  LP4;
               END CHECK;

               FUNCTION CHECK (LP : LPACC) RETURN RESULTS IS
               BEGIN
                    RETURN  LP5;
               END CHECK;
          
          BEGIN              
               IF CHECK (LPINT'(F)) /= LP1 THEN
                    FAILED ( "INCORRECT RESULTS FOR TYPE LPINT" );
               END IF;

               IF CHECK (LPCHAR'(F)) /= LP2 THEN
                    FAILED ( "INCORRECT RESULTS FOR TYPE LPCHAR" );
               END IF;
                 
               IF CHECK (LPARR'(F)) /= LP3 THEN
                    FAILED ( "INCORRECT RESULTS FOR TYPE LPARR" );
               END IF;
                 
               IF CHECK (LPREC'(F)) /= LP4 THEN
                    FAILED ( "INCORRECT RESULTS FOR TYPE LPREC" );
               END IF;

               IF CHECK (LPACC'(F)) /= LP5 THEN
                    FAILED ( "INCORRECT RESULTS FOR TYPE LPACC" );
               END IF;
                 
          END PKG2;

     BEGIN
          NULL;
     END;

     RESULT;
END C47002D;
