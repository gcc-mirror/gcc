-- A55B14A.ADA

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
-- USING A  CASE_STATEMENT , CHECK THAT THE SUBTYPE BOUNDS ASSOCIATED
--    WITH A LOOP OF THE FORM
--               FOR  I  IN  ST  LOOP
--    ARE, RESPECTIVELY,  ST'FIRST..ST'LAST WHEN ST IS STATIC.

-- RM 04/07/81
-- SPS 3/2/83
-- JBG 3/14/83

WITH REPORT;
PROCEDURE  A55B14A  IS

     USE REPORT;
     USE ASCII ;

     TYPE  ENUMERATION  IS  ( A,B,C,D,MIDPOINT,E,F,G,H );
     SUBTYPE   ST_I  IS       INTEGER RANGE 1..5       ;
     TYPE  NEW_ST_I  IS   NEW INTEGER RANGE 1..5       ;
     SUBTYPE   ST_E  IS       ENUMERATION RANGE B..G   ;
     SUBTYPE   ST_B  IS       BOOLEAN RANGE FALSE..FALSE;
     SUBTYPE   ST_C  IS       CHARACTER RANGE 'A'..DEL ;

BEGIN

     TEST("A55B14A" , "CHECK THAT THE SUBTYPE OF A LOOP PARAMETER"     &
                      " IN A LOOP OF THE FORM  'FOR  I  IN  ST  LOOP'" &
                      " ARE CORRECTLY DETERMINED WHEN ST IS STATIC" );

     BEGIN


          FOR  I  IN  ST_I  LOOP

               CASE  I  IS
                    WHEN  1 | 3 | 5  =>  NULL;
                    WHEN  2 | 4      =>  NULL;
               END CASE;

          END LOOP;


          FOR  I  IN  NEW_ST_I  LOOP

               CASE  I  IS
                    WHEN  1 | 3 | 5  =>  NULL;
                    WHEN  2 | 4      =>  NULL;
               END CASE;

          END LOOP;


          FOR  I  IN  ST_B  LOOP

               CASE  I  IS
                    WHEN  FALSE  =>  NULL;
               END CASE;

          END LOOP;


          FOR  I  IN  ST_C  LOOP

               CASE  I  IS
                    WHEN  'A'..'U'  =>  NULL;
                    WHEN  'V'..DEL  =>  NULL;
               END CASE;

          END LOOP;


          FOR  I  IN  ST_E  LOOP

               CASE  I  IS
                    WHEN  B..D      =>  NULL;
                    WHEN  E..G      =>  NULL;
                    WHEN  MIDPOINT  =>  NULL;
               END CASE;

          END LOOP;


     END;


     RESULT;


END A55B14A;
