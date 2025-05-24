-- C43003A.ADA

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
-- CHECK THAT WHEN INITIALIZING AN ARRAY OF ACCESS OBJECTS, WITH
-- AN AGGREGATE CONTAINING A SINGLE ALLOCATOR, ALL ELEMENTS
-- ARE INITIALIZED TO THE SAME INITIAL VALUE.
-- THAT IS, CHECK THAT ALL COMPONENTS OF THE ARRAY DESIGNATE
-- DISTINCT OBJECTS.

-- DAT 3/18/81
-- SPS 10/26/82
-- JBG 12/27/82
-- R. WILLIAMS 11/11/86     RENAMED FROM C38007A-B.ADA.

WITH REPORT; USE REPORT;

PROCEDURE C43003A IS

     TYPE AI IS ACCESS INTEGER;

     TYPE AAI IS ARRAY (1..5) OF AI;

     A : AAI := AAI'(OTHERS => NEW INTEGER '(2));

BEGIN
     TEST ("C43003A", "CHECK THAT ALLOCATORS IN INITIALIZATIONS"
          & " FOR ARRAYS OF ACCESS VALUES ARE EVALUATED ONCE" &
          " FOR EACH COMPONENT");

     FOR I IN 1..5
     LOOP
          FOR J IN I+1..5
          LOOP
               IF A(I) = A(J) THEN
                    FAILED ("DID NOT EVALUATE ALLOCATOR FOR EACH " &
                            "COMPONENT");
                    EXIT;
               END IF;
          END LOOP;
     END LOOP;

     RESULT;
END C43003A;
