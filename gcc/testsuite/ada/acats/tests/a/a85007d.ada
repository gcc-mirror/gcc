-- A85007D.ADA

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
-- CHECK THAT 'FIRST, 'LAST, 'LENGTH, 'RANGE, 'ADDRESS, 'CONSTRAINED, 
-- AND 'SIZE CAN BE APPLIED TO RENAMED NON-ACCESS OUT FORMAL PARAMETERS
-- AND RENAMED COMPONENTS OF NON-ACCESS OUT PARAMETERS. 

-- SPS 02/21/84 (SEE A62006D-B.ADA)
-- EG  02/22/84
-- EG  05/30/84
-- JBG 12/2/84

WITH REPORT; USE REPORT;
WITH SYSTEM;

PROCEDURE A85007D IS
     
     PROCEDURE Q (X : SYSTEM.ADDRESS) IS
     BEGIN
          NULL;
     END Q;

BEGIN

     TEST ("A85007D", "CHECK THAT ATTRIBUTES MAY BE APPLIED TO " &
           "RENAMED NON-ACCESS FORMAL OUT PARAMETERS");

     DECLARE

          TYPE ARR IS ARRAY (1 .. 2) OF BOOLEAN;
          TYPE REC (D : INTEGER) IS RECORD
               Y : BOOLEAN;
               X : ARR;
          END RECORD;

          PROCEDURE PROC (C2 : OUT ARR;
                          C3 : OUT REC) IS

               X : SYSTEM.ADDRESS;
               I : INTEGER;

               C21 : ARR RENAMES C2;
               C22 : ARR RENAMES C21;
               C31 : REC RENAMES C3;
               C32 : REC RENAMES C31;
               C33 : ARR RENAMES C3.X;
               C34 : ARR RENAMES C33;
               C35 : ARR RENAMES C32.X;
               C36 : BOOLEAN RENAMES C3.Y;
               C37 : BOOLEAN RENAMES C36;
               C38 : BOOLEAN RENAMES C32.Y;

          BEGIN

               I := C21'LENGTH;
               Q(C21'ADDRESS);
               I := C21'SIZE;
               I := C22'LENGTH;
               Q(C22'ADDRESS);
               I := C22'SIZE;

               FOR I IN C21'RANGE LOOP
                    NULL;
               END LOOP;
               FOR I IN C22'RANGE LOOP
                    NULL;
               END LOOP;

               FOR I IN C21'FIRST..C21'LAST LOOP
                    NULL;
               END LOOP;
               FOR I IN C22'FIRST..C22'LAST LOOP
                    NULL;
               END LOOP;

               I := C31.X'LENGTH;
               C3.Y := C31'CONSTRAINED;        
               FOR J IN C31.X'RANGE LOOP
                    NULL;
               END LOOP;
               FOR J IN C31.X'FIRST..C31.X'LAST LOOP
                    NULL;
               END LOOP;
               I := C32.X'LENGTH;
               C31.Y := C32'CONSTRAINED;
               FOR J IN C32.X'RANGE LOOP
                    NULL;
               END LOOP;
               FOR J IN C32.X'FIRST..C32.X'LAST LOOP
                    NULL;
               END LOOP;
               I := C33'LENGTH;
               FOR J IN C33'RANGE LOOP
                    NULL;
               END LOOP;
               FOR J IN C33'FIRST..C33'LAST LOOP
                    NULL;
               END LOOP;
               I := C34'LENGTH;
               FOR J IN C34'RANGE LOOP
                    NULL;
               END LOOP;
               FOR J IN C34'FIRST..C34'LAST LOOP
                    NULL;
               END LOOP;
               I := C35'LENGTH;
               FOR J IN C35'RANGE LOOP
                    NULL;
               END LOOP;
               FOR J IN C35'FIRST..C35'LAST LOOP
                    NULL;
               END LOOP;

               Q(C31.Y'ADDRESS);
               I := C31.Y'SIZE;
               Q(C32.Y'ADDRESS);
               I := C32.Y'SIZE;
               Q(C36'ADDRESS);
               I := C36'SIZE;
               Q(C37'ADDRESS);
               I := C37'SIZE;
               Q(C38'ADDRESS);
               I := C38'SIZE;

          END PROC;

     BEGIN

          NULL;

     END;

     RESULT;

END A85007D;
