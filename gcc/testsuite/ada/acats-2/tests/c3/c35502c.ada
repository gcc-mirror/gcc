-- C35502C.ADA

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
-- CHECK THAT THE ATTRIBUTES 'IMAGE' AND 'VALUE' YIELD THE CORRECT 
-- RESULTS WHEN THE PREFIX IS AN ENUMERATION TYPE OTHER THAN A BOOLEAN
-- OR A CHARACTER TYPE.   
-- SUBTESTS ARE:
--     PART (A). TESTS FOR IMAGE.
--     PART (B). TESTS FOR VALUE.

-- RJW 5/07/86

WITH REPORT; USE REPORT;

PROCEDURE  C35502C  IS

          TYPE ENUM IS (A, BC, ABC, A_B_C, abcd);
          SUBTYPE SUBENUM IS ENUM RANGE A .. BC;

          TYPE NEWENUM IS NEW ENUM;

          FUNCTION IDENT (X : ENUM) RETURN ENUM IS
               BEGIN
                    IF EQUAL (ENUM'POS (X), ENUM'POS(X)) THEN
                         RETURN X;
                    END IF;
                    RETURN ENUM'FIRST;
               END IDENT;       

BEGIN

     TEST( "C35502C" , "CHECK THAT THE ATTRIBUTES 'IMAGE' AND " &
                       "'VALUE' YIELD THE CORRECT RESULTS " &
                       "WHEN THE PREFIX IS AN ENUMERATION TYPE " &
                       "OTHER THAN A BOOLEAN OR A CHARACTER TYPE" );

-- PART (A).

     BEGIN  

          IF ENUM'IMAGE ( IDENT(ABC) ) /= "ABC" THEN
               FAILED ( "INCORRECT ENUM'IMAGE FOR ABC" );
          END IF;
          IF ENUM'IMAGE ( IDENT(ABC) )'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR ABC IN ENUM" );
          END IF;

          IF ENUM'IMAGE ( IDENT(A_B_C) ) /= "A_B_C" THEN
               FAILED ( "INCORRECT ENUM'IMAGE FOR A_B_C" );
          END IF;
          IF ENUM'IMAGE ( IDENT(A_B_C) )'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR A_B_C IN ENUM" );
          END IF;

          IF SUBENUM'IMAGE ( IDENT(A_B_C) ) /= "A_B_C" THEN
               FAILED ( "INCORRECT SUBENUM'IMAGE FOR A_B_C" );
          END IF;          
          IF SUBENUM'IMAGE ( IDENT(ABC) )'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR ABC " &
                        "IN SUBENUM" );
          END IF;          

          IF NEWENUM'IMAGE ( ABC ) /= IDENT_STR("ABC") THEN
               FAILED ( "INCORRECT NEWENUM'IMAGE FOR ABC" );
          END IF;          
          IF NEWENUM'IMAGE ( ABC )'FIRST /= IDENT_INT(1) THEN
               FAILED ( "INCORRECT LOWER BOUND FOR ABC" &
                        "IN NEWENUM" );
          END IF;          

          IF ENUM'IMAGE ( IDENT(abcd) ) /= "ABCD" THEN
               FAILED ( "INCORRECT ENUM'IMAGE FOR abcd" );
          END IF;          
          IF ENUM'IMAGE ( IDENT(abcd) )'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR abcd IN ENUM" );
          END IF;          

     END;

-----------------------------------------------------------------------

-- PART (B).

     BEGIN  
          IF ENUM'VALUE (IDENT_STR("ABC")) /= ABC THEN     
               FAILED ( "INCORRECT VALUE FOR ""ABC""" );  
          END IF;
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE FOR ""ABC""" );  
     END;

     BEGIN
          IF ENUM'VALUE (IDENT_STR("abc")) /= abc THEN     
               FAILED ( "INCORRECT VALUE FOR ""abc""" );  
          END IF;
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE FOR ""abc""" );  
     END;

     BEGIN
          IF ENUM'VALUE ("ABC") /= ABC THEN     
               FAILED ( "INCORRECT VALUE FOR ABC" );  
          END IF;
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE FOR ABC" );  
     END;

     BEGIN
          IF NEWENUM'VALUE (IDENT_STR("abcd")) /= abcd THEN
               FAILED ( "INCORRECT VALUE FOR ""abcd""" );
          END IF;          
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE FOR ""abcd""" );
     END;

     BEGIN
          IF NEWENUM'VALUE (IDENT_STR("ABCD")) /= abcd THEN
               FAILED ( "INCORRECT VALUE FOR ""ABCD""" );
          END IF;          
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE FOR ""ABCD""" );
     END;

     BEGIN
          IF NEWENUM'VALUE ("abcd") /= abcd THEN
               FAILED ( "INCORRECT VALUE FOR abcd" );
          END IF;          
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE FOR abcd" );
     END;

     BEGIN
          IF SUBENUM'VALUE (IDENT_STR("A_B_C")) /= A_B_C THEN
               FAILED ( "INCORRECT VALUE FOR ""A_B_C""" );
          END IF;          
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE FOR ""A_B_C""" );
     END;

     BEGIN
          IF ENUM'VALUE (IDENT_STR("ABC     ")) /= ABC THEN
               FAILED ( "INCORRECT VALUE WITH TRAILING BLANKS" );
          END IF;          
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE WITH " &
                        "TRAILING BLANKS" );
     END;

     BEGIN          
          IF NEWENUM'VALUE (IDENT_STR("  A_B_C")) /= A_B_C THEN
               FAILED ( "INCORRECT VALUE WITH LEADING BLANKS" );
          END IF;          
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE WITH LEADING " &
                        "BLANKS" );
     END;

     BEGIN
          IF ENUM'VALUE (IDENT_STR("A_BC")) /= ABC THEN
               FAILED ( "NO EXCEPTION RAISED - ""A_BC"" - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - ""A_BC"" - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - ""A_BC""" );
     END;

     BEGIN
          IF ENUM'VALUE (IDENT_STR("A BC")) /= ABC THEN
               FAILED ( "NO EXCEPTION RAISED - ""A BC"" - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - ""A BC"" - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - ""A BC""" );
     END;

     BEGIN
          IF ENUM'VALUE (IDENT_STR("A&BC")) /= ABC THEN
               FAILED ( "NO EXCEPTION RAISED - ""A&BC"" - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - ""A&BC"" - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - ""A&BC""" );
     END;

     BEGIN
          IF ENUM'VALUE (IDENT_CHAR(ASCII.HT) & "BC") /= BC THEN
               FAILED ( "NO EXCEPTION RAISED - LEADING 'HT' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - LEADING 'HT' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - LEADING 'HT'" );
     END;

     BEGIN
          IF NEWENUM'VALUE ("A" & (IDENT_CHAR(ASCII.HT))) /= A THEN
               FAILED ( "NO EXCEPTION RAISED - TRAILING 'HT' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - TRAILING 'HT' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - TRAILING 'HT'" );
     END;

     BEGIN
          IF ENUM'VALUE (IDENT_STR("B__C")) /= BC THEN
               FAILED ( "NO EXCEPTION RAISED - " &
                        "CONSECUTIVE UNDERSCORES - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - " &
                        "CONSECUTIVE UNDERSCORES - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - " &
                        "CONSECUTIVE UNDERSCORES" );
     END;

     BEGIN
          IF NEWENUM'VALUE (IDENT_STR("BC_")) /= BC THEN
               FAILED ( "NO EXCEPTION RAISED - " & 
                        "TRAILING UNDERSCORE - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - " & 
                        "TRAILING UNDERSCORE - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - " & 
                        "TRAILING UNDERSCORE" );
     END;

     BEGIN
          IF SUBENUM'VALUE (IDENT_STR("_BC")) /= BC THEN
               FAILED ( "NO EXCEPTION RAISED - " & 
                        "LEADING UNDERSCORE - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - " & 
                        "LEADING UNDERSCORE - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - " & 
                        "LEADING UNDERSCORE" );
     END;

     BEGIN
          IF SUBENUM'VALUE (IDENT_STR("0BC")) /= BC THEN
               FAILED ( "NO EXCEPTION RAISED - " & 
                        "FIRST CHARACTER IS A DIGIT - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - " & 
                        "FIRST CHARACTER IS A DIGIT - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - " & 
                        "FIRST CHARACTER IS A DIGIT" );
     END;

     RESULT;
END C35502C;
