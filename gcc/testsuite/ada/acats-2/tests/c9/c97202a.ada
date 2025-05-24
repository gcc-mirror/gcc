-- C97202A.ADA

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
-- CHECK THAT THE INDEX IS EVALUATED BEFORE THE ENTRY PARAMETER AND BOTH
-- THE INDEX AND THE ENTRY PARAMETER ARE EVALUATED BEFORE THE RENDEZVOUS
-- IS ATTEMPED.

-- RM 4/05/82
-- TBN 2/3/86     ADDED A CHECK THAT INDEX IS EVALUATED BEFORE THE ENTRY
--                PARAMETER AND FIXED APPROPRIATE COMMENTS.

WITH REPORT; USE REPORT;
PROCEDURE C97202A IS

     INDEX_COMPUTED   :  BOOLEAN  :=  FALSE ;
     FORMAL_COMPUTED  :  BOOLEAN  :=  FALSE ;

BEGIN

     TEST ("C97202A", "CHECK THAT THE INDEX IS EVALUATED BEFORE THE " &
                      "ENTRY PARAMETER AND BOTH INDEX AND THE ENTRY " &
                      "PARAMETER ARE EVALUATED BEFORE THE RENDEZVOUS " &
                      "IS ATTEMPTED");

     DECLARE
          SUBTYPE  SHORT  IS  INTEGER RANGE 10..20 ;

          TASK  T  IS
               ENTRY  DO_IT_NOW_ORELSE (SHORT)
                                       (DID_YOU_DO_IT : IN BOOLEAN);
               ENTRY  KEEP_ALIVE ;
          END  T ;

          TASK BODY  T  IS
          BEGIN
               ACCEPT  KEEP_ALIVE ;
          END  T ;

          FUNCTION  F1 (X:INTEGER) RETURN INTEGER  IS
          BEGIN
               IF FORMAL_COMPUTED THEN
                    FAILED ("INDEX WAS NOT EVALUATED FIRST");
               END IF;
               INDEX_COMPUTED  :=  TRUE ;
               RETURN (7) ;
          END  F1 ;

          FUNCTION  F2 (X:INTEGER) RETURN BOOLEAN  IS
          BEGIN
               FORMAL_COMPUTED  :=  TRUE ;
               RETURN (FALSE) ;
          END  F2 ;

     BEGIN
          SELECT
               T.DO_IT_NOW_ORELSE ( 6 + F1(7)  )
                                  ( NOT(F2(7)) ) ;
          ELSE
               NULL ;
          END SELECT;

          T.KEEP_ALIVE ;
     END;   -- END OF BLOCK CONTAINING THE ENTRY CALLS.

     IF  INDEX_COMPUTED  THEN
          NULL ;
     ELSE
          FAILED( "ENTRY INDEX WAS NOT COMPUTED" );
     END IF;

     IF  FORMAL_COMPUTED  THEN
          NULL ;
     ELSE
          FAILED( "ENTRY PARAMETER WAS NOT COMPUTED" );
     END IF;

     RESULT;

END  C97202A ;
