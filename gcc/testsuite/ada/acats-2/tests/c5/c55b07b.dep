-- C55B07B.DEP

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
--     CHECK THAT LOOPS OVER RANGES OF TYPE SHORT_INTEGER
--     CAN BE WRITTEN.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     THE TYPE SHORT_INTEGER.
--
--     IF THE TYPE SHORT_INTEGER IS NOT SUPPORTED, THEN THE
--     DECLARATION OF CHECK MUST BE REJECTED.

-- HISTORY:
--     RM  07/08/82  CREATED ORIGINAL TEST.
--     BCB 01/04/88  MODIFIED HEADER.


WITH REPORT; USE REPORT;

PROCEDURE C55B07B IS

     CHECK : SHORT_INTEGER;                            -- N/A => ERROR.

     TYPE  NEW_SHORT_INTEGER  IS  NEW SHORT_INTEGER ;

     THE_COUNT : INTEGER := 777 ;   -- JUST A DUMMY...

     SI_VAR   :           SHORT_INTEGER      :=  1 ;
     SI_CON   :  CONSTANT SHORT_INTEGER      :=  1 ;

     NSI_VAR  :           NEW_SHORT_INTEGER  :=  1 ;
     NSI_CON  :  CONSTANT NEW_SHORT_INTEGER  :=  1 ;

     SUBTYPE   SI_SEGMENT  IS  SHORT_INTEGER RANGE
                               SHORT_INTEGER'LAST..SHORT_INTEGER'LAST ;

     SUBTYPE  NSI_SEGMENT  IS  NEW_SHORT_INTEGER RANGE
                               NEW_SHORT_INTEGER'FIRST..
                               NEW_SHORT_INTEGER'FIRST ;

     COUNT : INTEGER := 0;

     PROCEDURE  BUMP ( DUMMY : INTEGER )  IS
     BEGIN
          COUNT := COUNT + 1;
     END  BUMP;

BEGIN

     TEST ( "C55B07B" , "LOOPS OVER RANGES OF TYPE  SHORT_INTEGER " );

     FOR  I  IN  1..SI_CON  LOOP
          BUMP(THE_COUNT) ;
     END LOOP;

     FOR  I  IN  NSI_VAR..1  LOOP
          BUMP(THE_COUNT) ;
     END LOOP;

     FOR  I  IN  1..SHORT_INTEGER(1)  LOOP
          BUMP(THE_COUNT) ;
     END LOOP;

     FOR  I  IN  1..NEW_SHORT_INTEGER(1)  LOOP
          BUMP(THE_COUNT) ;
     END LOOP;

     FOR  I  IN  SI_SEGMENT  LOOP
          BUMP(THE_COUNT) ;
     END LOOP;

     FOR  I  IN  REVERSE NSI_SEGMENT  LOOP
          BUMP(THE_COUNT) ;
     END LOOP;

     FOR  I  IN  SHORT_INTEGER RANGE 1..1  LOOP
          BUMP(THE_COUNT) ;
     END LOOP;

     FOR  I  IN  NEW_SHORT_INTEGER RANGE 1..1  LOOP
          BUMP(THE_COUNT) ;
     END LOOP;

     FOR  I  IN  SHORT_INTEGER LOOP
          BUMP(THE_COUNT) ;
          EXIT WHEN  I = SHORT_INTEGER'FIRST + 1;
     END LOOP;

     FOR  I  IN  NEW_SHORT_INTEGER LOOP
          BUMP(THE_COUNT) ;
          EXIT WHEN  I = NEW_SHORT_INTEGER'FIRST + 1;
     END LOOP;


     IF  COUNT /= 12  THEN
          FAILED ("WRONG LOOP COUNT");
     END IF;


     RESULT;


END  C55B07B ;
