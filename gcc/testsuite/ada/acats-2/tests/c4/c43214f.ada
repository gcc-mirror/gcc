-- C43214F.ADA

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
-- CHECK THAT THE LOWER BOUND FOR THE STRING LITERAL IS DETERMINED BY
-- THE APPLICABLE INDEX CONSTRAINT, WHEN ONE EXISTS.

-- EG  02/10/84
-- JBG 3/30/84

WITH REPORT;

PROCEDURE C43214F IS

     USE REPORT;

BEGIN

     TEST("C43214F", "ARRAY COMPONENT EXPRESSION OF AN ENCLOSING " &
                     "AGGREGATE");

     BEGIN

CASE_E :  BEGIN

--             COMMENT ("CASE E1 : ARRAY COMPONENT EXPRESSION OF " &
--                      "AN ENCLOSING ARRAY AGGREGATE");

     CASE_E1 : DECLARE

                    TYPE TE2 IS ARRAY(1 .. 2) OF 
                                STRING(IDENT_INT(3) .. 5);

                    E1 : TE2;

               BEGIN

                    E1 := (1 .. 2 => "ABC");
                    IF (E1'FIRST /= 1 OR E1'LAST /= 2) OR ELSE
                       (E1(1)'FIRST /= 3 OR E1(1)'LAST /= 5 OR
                        E1(2)'FIRST /= 3 OR E1(2)'LAST /= 5) THEN
                         FAILED ("CASE 1 : INCORRECT BOUNDS");
                    ELSIF E1 /= (1 .. 2 => "ABC") THEN
                         FAILED ("CASE 1 : ARRAY DOES NOT " &
                                 "CONTAIN THE CORRECT VALUES");
                    END IF;

               END CASE_E1;

--             COMMENT ("CASE E2 : ARRAY COMPONENT EXPRESSION OF " &
--                      "AN ENCLOSING RECORD AGGREGATE");

     CASE_E2 : DECLARE

                    TYPE TER IS
                         RECORD
                              REC : STRING(3 .. 5);
                         END RECORD;

                    E2 : TER;

               BEGIN

                    E2 := (REC => "ABC");
                    IF E2.REC'FIRST /= 3 OR E2.REC'LAST /= 5 THEN
                         FAILED ("CASE 2 : INCORRECT BOUNDS");
                    ELSIF E2.REC /= "ABC" THEN
                         FAILED ("CASE 2 : ARRAY DOES NOT " &
                                 "CONTAIN CORRECT VALUES");
                    END IF;

               END CASE_E2;

--             COMMENT ("CASE E3 : NULL LITERAL OF AN ENCLOSING " &
--                      "ARRAY AGGREGATE");

     CASE_E3 : DECLARE

                    TYPE TE2 IS ARRAY(1 .. 2) OF
                                STRING(3 .. IDENT_INT(2));

                    E3 : TE2;

               BEGIN

                    E3 := (1 .. 2 => "");
                    IF (E3'FIRST /= 1 OR E3'LAST /= 2) OR ELSE
                       (E3(1)'FIRST /= 3 OR E3(1)'LAST /= 2 OR
                        E3(2)'FIRST /= 3 OR E3(2)'LAST /= 2) THEN
                         FAILED ("CASE 3 : INCORRECT BOUND");
                    ELSIF E3 /= (1 .. 2 => "") THEN
                         FAILED ("CASE 3 : ARRAY DOES NOT CONTAIN " &
                                 "THE CORRECT VALUES");
                    END IF;

               END CASE_E3;

--             COMMENT ("CASE E4 : ARRAY COMPONENT EXPRESSION OF "  &
--                      "AN ENCLOSING RECORD AGGREGATE THAT HAS A " &
--                      "DISCRIMINANT AND THE DISCRIMINANT DETER"   &
--                      "MINES THE BOUNDS OF THE COMPONENT");

     CASE_E4 : DECLARE

                    SUBTYPE TEN IS INTEGER RANGE 1 .. 10;
                    TYPE TER (A : TEN) IS
                         RECORD
                              REC : STRING(3 .. A);
                         END RECORD;

                    E4 : TER(5);

               BEGIN

                    E4 := (REC => "ABC", A => 5);
                    IF E4.REC'FIRST /= 3 OR E4.REC'LAST /= 5 THEN
                         FAILED ("CASE 4 : INCORRECT BOUNDS");
                    ELSIF E4.REC /= "ABC" THEN
                         FAILED ("CASE 4 : ARRAY DOES NOT CONTAIN " &
                                 "CORRECT VALUES");
                    END IF;

               END CASE_E4;

          END CASE_E;

     END;

     RESULT;

END C43214F;
