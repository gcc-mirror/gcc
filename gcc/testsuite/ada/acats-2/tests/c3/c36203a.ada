-- C36203A.ADA

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
-- CHECK THAT 'LENGTH YIELDS A RESULT OF TYPE UNIVERSAL INTEGER.

-- L.BROWN  07/31/86

WITH REPORT; USE REPORT;
PROCEDURE C36203A IS

     TYPE NINT IS NEW INTEGER RANGE 1 .. 5;

     TYPE INT_ARR IS ARRAY(INTEGER RANGE 1 .. 3) OF INTEGER;
     TYPE INT2_ARR IS ARRAY(INTEGER RANGE 1 .. 3,
                            INTEGER RANGE 1 .. 2) OF INTEGER;

     OBJA : INTEGER := 3;
     OBJB : NINT := 3;

BEGIN
     TEST("C36203A", "'LENGTH YIELDS A RESULT OF TYPE " &
                     "UNIVERSAL INTEGER");
     IF (OBJA + INT_ARR'LENGTH) /= IDENT_INT(6)  THEN
          FAILED("INCORRECT VALUE RETURNED BY 'LENGTH " &
                 "FOR ONE-DIM ARRAY TYPE 1");
     END IF;

     IF (OBJB + INT_ARR'LENGTH) /= 6  THEN
          FAILED("INCORRECT VALUE RETURNED BY 'LENGTH " &
                 "FOR ONE-DIM ARRAY TYPE 2");
     END IF;

     IF (OBJA + INT2_ARR'LENGTH(1)) /= IDENT_INT(6)  THEN
          FAILED("INCORRECT VALUE RETURNED BY 'LENGTH " &
                 "FOR FIRST DIMENSION OF TWO-DIM ARRAY TYPE 1");
     END IF;

     IF (OBJB + INT2_ARR'LENGTH(1)) /= 6  THEN
          FAILED("INCORRECT VALUE RETURNED BY 'LENGTH " &
                 "FOR FIRST DIMENSION OF TWO-DIM ARRAY TYPE 2");
     END IF;

     IF (OBJA + INT2_ARR'LENGTH(2)) /= IDENT_INT(5)  THEN
          FAILED("INCORRECT VALUE RETURNED BY 'LENGTH " &
                 "FOR SECOND DIMENSION OF TWO-DIM ARRAY TYPE 1");
     END IF;

     IF (OBJB + INT2_ARR'LENGTH(2)) /= 5  THEN
          FAILED("INCORRECT VALUE RETURNED BY 'LENGTH " &
                 "FOR SECOND DIMENSION OF TWO-DIM ARRAY TYPE 2");
     END IF;

     RESULT;

END C36203A;
