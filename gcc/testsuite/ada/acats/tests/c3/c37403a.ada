-- C37403A.ADA

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
-- CHECK THAT WHEN A FORMAL PARAMETER OF A SUBPROGRAM, ENTRY, OR 
-- GENERIC UNIT HAS AN UNCONSTRAINED TYPE WITH DISCRIMINANTS THAT DO 
-- NOT HAVE DEFAULTS, 'CONSTRAINED IS 'TRUE' REGARDLESS OF THE MODE 
-- OF THE PARAMETER.

-- R.WILLIAMS 9/1/86

WITH REPORT; USE REPORT;
PROCEDURE C37403A IS

BEGIN
     TEST ( "C37403A", "CHECK THAT WHEN A FORMAL PARAMETER OF A " &
                       "SUBPROGRAM, ENTRY, OR GENERIC UNIT HAS AN " &
                       "UNCONSTRAINED TYPE WITH DISCRIMINANTS THAT " &
                       "DO NOT HAVE DEFAULTS, 'CONSTRAINED IS " &
                       "'TRUE' REGARDLESS OF THE MODE OF THE " &
                       "PARAMETER" );

     
     DECLARE

          SUBTYPE INT IS INTEGER RANGE 1.. 10;

          TYPE MATRIX IS ARRAY (INT RANGE <>, INT RANGE <>) 
               OF INTEGER;

          TYPE SQUARE (SIDE : INT) IS
               RECORD
                    MAT : MATRIX (1 .. SIDE, 1 .. SIDE);
               END RECORD;

          S1 : SQUARE (2) := (2, ((1, 2), (3, 4)));

          S2 : SQUARE (2) := S1;

          S3 : SQUARE (2);
 
          SC : CONSTANT SQUARE := (SIDE => 1, MAT => (1 => (1 => 1)));

          PROCEDURE P (PIN1, PIN2 : IN     SQUARE; 
                       PINOUT     : IN OUT SQUARE;
                       POUT       : OUT    SQUARE) IS

          BEGIN
               IF PIN1'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR OBJECT " &
                             "OF IN MODE - 1" );
               END IF;

               IF PIN2'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR OBJECT " &
                             "OF IN MODE - 2" );
               END IF;

               IF PINOUT'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                             "OBJECT OF IN OUT MODE - 1" );
               END IF;

               IF POUT'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                             "OBJECT OF OUT MODE - 1" );
               END IF;

               POUT := (2, ((1, 2), (3, 4)));
          END P;

          TASK T IS 
               ENTRY Q (PIN1, PIN2  : IN     SQUARE; 
                        PINOUT      : IN OUT SQUARE;
                        POUT        : OUT    SQUARE);
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT Q (PIN1, PIN2  : IN     SQUARE; 
                         PINOUT      : IN OUT SQUARE;
                         POUT        : OUT    SQUARE) DO

                    BEGIN
                         IF PIN1'CONSTRAINED THEN
                              NULL;
                         ELSE
                              FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                                       "OBJECT OF IN MODE - 3" );
                         END IF;

                         IF PIN2'CONSTRAINED THEN
                              NULL;
                         ELSE
                              FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                                       "OBJECT OF IN MODE - 4" );
                         END IF;
     
                         IF PINOUT'CONSTRAINED THEN
                              NULL;
                         ELSE
                              FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                                       "OBJECT OF " &
                                       "IN OUT MODE - 2" );
                         END IF;
          
                         IF POUT'CONSTRAINED THEN
                              NULL;
                         ELSE
                              FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                                       "OBJECT OF " &
                                       "OUT MODE - 2" );
                         END IF;

                         POUT := (2, ((1, 2), (3, 4)));
                    END;
               END Q;
          END T;

          GENERIC
               PIN1, PIN2 : IN     SQUARE; 
               PINOUT     : IN OUT SQUARE;
          PACKAGE R IS END R;

          PACKAGE BODY R IS
          BEGIN
               IF PIN1'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR OBJECT " &
                             "OF IN MODE - 5" );
               END IF;

               IF PIN2'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR OBJECT " &
                             "OF IN MODE - 6" );
               END IF;

               IF PINOUT'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                             "OBJECT OF IN OUT MODE - 3" );
               END IF;

          END R;

          PACKAGE S IS NEW R (S1, SC, S2);

     BEGIN
          P (S1, SC, S2, S3);
          T.Q (S1, SC, S2, S3);
     END;
          
     RESULT;
END C37403A;
