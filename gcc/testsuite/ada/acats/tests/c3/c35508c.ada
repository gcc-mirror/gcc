-- C35508C.ADA

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
-- RESULTS WHEN THE PREFIX IS A BOOLEAN TYPE.   

-- SUBTESTS ARE:
--     (A). TESTS FOR IMAGE.
--     (B). TESTS FOR VALUE.

-- RJW 3/19/86

WITH REPORT; USE REPORT;

PROCEDURE  C35508C  IS     

     TYPE NEWBOOL IS NEW BOOLEAN;

BEGIN

     TEST( "C35508C" , "CHECK THAT THE ATTRIBUTES 'IMAGE' AND " &
                       "'VALUE' YIELD THE CORRECT RESULTS WHEN THE " &
                       "PREFIX IS A BOOLEAN TYPE" );
-- PART (A).

     DECLARE

          A5, B5 : INTEGER := IDENT_INT(5);
          C6     : INTEGER := IDENT_INT(6);
     BEGIN  

          IF BOOLEAN'IMAGE ( A5 = B5 ) /= "TRUE" THEN
               FAILED ( "INCORRECT IMAGE FOR 'A5 = B5'" );
          END IF;
          IF BOOLEAN'IMAGE ( A5 = B5 )'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR 'A5 = B5'" );
          END IF;

          IF BOOLEAN'IMAGE ( C6 = A5 ) /= "FALSE" THEN
               FAILED ( "INCORRECT IMAGE FOR 'C6 = A5'" );
          END IF;          
          IF BOOLEAN'IMAGE ( C6 = A5 )'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR 'C6 = A5'" );
          END IF;          

          IF BOOLEAN'IMAGE (TRUE) /= "TRUE" THEN
               FAILED ( "INCORRECT IMAGE FOR 'TRUE'" );
          END IF;          
          IF BOOLEAN'IMAGE (TRUE)'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR 'TRUE'" );
          END IF;          

          IF NEWBOOL'IMAGE (FALSE) /= "FALSE" THEN
               FAILED ( "INCORRECT IMAGE FOR NEWBOOL'FALSE'" );
          END IF;          
          IF NEWBOOL'IMAGE (FALSE)'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR NEWBOOL'FALSE'" );
          END IF;          
     END;

-----------------------------------------------------------------------

-- PART (B).

     BEGIN  
          IF BOOLEAN'VALUE (IDENT_STR("TRUE")) /= TRUE THEN     
               FAILED ( "INCORRECT VALUE FOR ""TRUE""" );  
          END IF;
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE FOR ""TRUE""" );  
     END;

     BEGIN
          IF NEWBOOL'VALUE (IDENT_STR("FALSE")) /= FALSE THEN
               FAILED ( "INCORRECT VALUE FOR ""FALSE""" );
          END IF;          
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE FOR ""FALSE""" );
     END;

     BEGIN
          IF BOOLEAN'VALUE ("true") /= TRUE THEN
               FAILED ( "INCORRECT VALUE FOR ""true""" );
          END IF;          
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE FOR ""true""" );
     END;

     BEGIN
          IF NEWBOOL'VALUE ("false") /= FALSE THEN
               FAILED ( "INCORRECT VALUE FOR ""false""" );
          END IF;          
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE FOR " &
                        """false""" );
     END;

     BEGIN
          IF BOOLEAN'VALUE (IDENT_STR("TRUE     ")) /= TRUE THEN
               FAILED ( "INCORRECT VALUE WITH TRAILING BLANKS" );
          END IF;          
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE - " &
                        "TRAILING BLANKS" );
     END;

     BEGIN          
          IF NEWBOOL'VALUE ("  FALSE") /= FALSE THEN
               FAILED ( "INCORRECT VALUE WITH LEADING BLANKS" );
          END IF;          
     EXCEPTION     
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - VALUE - LEADING " &
                        "BLANKS" );
     END;

     DECLARE 
          SUBTYPE SUBBOOL IS BOOLEAN RANGE FALSE .. FALSE;
     BEGIN
          IF SUBBOOL'VALUE (IDENT_STR("TRUE")) /= TRUE THEN     
               FAILED ( "INCORRECT VALUE - ""TRUE"" AND " & 
                        "SUBBOOL" );  
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - SUBBOOL" );
     END;
     
     BEGIN
          IF BOOLEAN'VALUE (IDENT_STR("MAYBE")) = TRUE THEN
               FAILED ( "NO EXCEPTION RAISED - ""MAYBE"" - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - ""MAYBE"" - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - ""MAYBE"" " );
     END;

     BEGIN
          IF BOOLEAN'VALUE (IDENT_CHAR(ASCII.HT) & "TRUE") = TRUE THEN
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
          IF NEWBOOL'VALUE ("FALSE" & ASCII.HT) = FALSE THEN
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

     RESULT;
END C35508C;
