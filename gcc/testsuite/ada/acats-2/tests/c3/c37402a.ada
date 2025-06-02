-- C37402A.ADA

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
-- GENERIC UNIT HAS AN UNCONSTRAINED TYPE WITH DISCRIMINANTS THAT 
-- HAVE DEFAULTS, 'CONSTRAINED IS 'TRUE' WHEN APPLIED TO FORMAL 
-- PARAMETERS OF MODE IN AND HAS THE VALUE OF THE ACTUAL PARAMETER 
-- FOR THE OTHER MODES.

-- R.WILLIAMS 9/1/86

WITH REPORT; USE REPORT;
PROCEDURE C37402A IS

BEGIN
     TEST ( "C37402A", "CHECK THAT WHEN A FORMAL PARAMETER OF A " &
                       "SUBPROGRAM, ENTRY, OR GENERIC UNIT HAS AN " &
                       "UNCONSTRAINED TYPE WITH DISCRIMINANTS THAT " &
                       "HAVE DEFAULTS, 'CONSTRAINED IS 'TRUE' WHEN " &
                       "APPLIED TO FORMAL PARAMETERS OF MODE IN " &
                       "AND HAS THE VALUE OF THE ACTUAL PARAMETER " &
                       "FOR THE OTHER MODES" );

     
     DECLARE

          SUBTYPE INT IS INTEGER RANGE 1 .. 5;

          TYPE MATRIX IS ARRAY (INT RANGE <>, INT RANGE <>) 
               OF INTEGER;

          TYPE SQUARE (SIDE : INT := 1) IS
               RECORD
                    MAT : MATRIX (1 .. SIDE, 1 .. SIDE);
               END RECORD;

          SC : CONSTANT SQUARE := (2, ((0, 0), (0, 0)));

          AC : SQUARE (2) := (2, ((1, 2), (3, 4)));
          AU : SQUARE     := (SIDE => 1, MAT => (1 => (1 => 1)));

          BC : SQUARE (2) := AC;
          BU : SQUARE     := AU;

          CC : SQUARE (2);
          CU : SQUARE;
 
          PROCEDURE P (CON, IN_CON : IN     SQUARE; 
                       INOUT_CON   : IN OUT SQUARE;
                       OUT_CON     : OUT    SQUARE; 
                       IN_UNC      : IN     SQUARE; 
                       INOUT_UNC   : IN OUT SQUARE; 
                       OUT_UNC     : OUT    SQUARE) IS

          BEGIN
               IF CON'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR OBJECT " &
                             "OF IN MODE - 1" );
               END IF;

               IF IN_CON'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR OBJECT " &
                             "OF IN MODE - 2" );
               END IF;

               IF IN_UNC'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR OBJECT " &
                             "OF IN MODE - 3" );
               END IF;

               IF INOUT_CON'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                             "CONSTRAINED OBJECT OF IN OUT MODE - 1" );
               END IF;

               IF OUT_CON'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                             "CONSTRAINED OBJECT OF OUT MODE - 1" );
               END IF;

               IF INOUT_UNC'CONSTRAINED THEN
                    FAILED ( "'CONSTRAINED IS 'TRUE' FOR " &
                             "UNCONSTRAINED OBJECT OF IN OUT MODE " &
                             "- 1" );
               END IF;

               IF OUT_UNC'CONSTRAINED THEN
                    FAILED ( "'CONSTRAINED IS 'TRUE' FOR " &
                             "UNCONSTRAINED OBJECT OF OUT MODE - 1" );
               END IF;
          
               OUT_CON := (2, ((1, 2), (3, 4)));
               OUT_UNC := (2, ((1, 2), (3, 4)));
          END P;

          TASK T IS 
               ENTRY Q (CON, IN_CON : IN     SQUARE; 
                        INOUT_CON   : IN OUT SQUARE;
                        OUT_CON     : OUT    SQUARE; 
                        IN_UNC      : IN     SQUARE; 
                        INOUT_UNC   : IN OUT SQUARE; 
                        OUT_UNC     : OUT    SQUARE);
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT Q (CON, IN_CON : IN     SQUARE; 
                         INOUT_CON   : IN OUT SQUARE;
                         OUT_CON     : OUT    SQUARE; 
                         IN_UNC      : IN     SQUARE; 
                         INOUT_UNC   : IN OUT SQUARE; 
                         OUT_UNC     : OUT    SQUARE) DO
                    BEGIN
                         IF CON'CONSTRAINED THEN
                              NULL;
                         ELSE
                              FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                                       "OBJECT OF IN MODE - 4" );
                         END IF;

                         IF IN_CON'CONSTRAINED THEN
                              NULL;
                         ELSE
                              FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                                       "OBJECT OF IN MODE - 5" );
                         END IF;

                         IF IN_UNC'CONSTRAINED THEN
                              NULL;
                         ELSE
                              FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                                       "OBJECT OF IN MODE - 6" );
                         END IF;
     
                         IF INOUT_CON'CONSTRAINED THEN
                              NULL;
                         ELSE
                              FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                                       "CONSTRAINED OBJECT OF " &
                                       "IN OUT MODE - 2" );
                         END IF;
          
                         IF OUT_CON'CONSTRAINED THEN
                              NULL;
                         ELSE
                              FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                                       "CONSTRAINED OBJECT OF " &
                                       "OUT MODE - 2" );
                         END IF;

                         IF INOUT_UNC'CONSTRAINED THEN
                              FAILED ( "'CONSTRAINED IS 'TRUE' FOR " &
                                       "UNCONSTRAINED OBJECT OF " &
                                       "IN OUT MODE - 2" );
                         END IF;
          
                         IF OUT_UNC'CONSTRAINED THEN
                              FAILED ( "'CONSTRAINED IS 'TRUE' FOR " &
                                       "UNCONSTRAINED OBJECT OF " &
                                       "OUT MODE - 2" );
                         END IF;
          
                         OUT_CON := (2, ((1, 2), (3, 4)));
                         OUT_UNC := (2, ((1, 2), (3, 4)));
                    END;
               END Q;
          END T;

          GENERIC
               CON, IN_CON : IN     SQUARE; 
               INOUT_CON   : IN OUT SQUARE;
               IN_UNC      : IN     SQUARE; 
               INOUT_UNC   : IN OUT SQUARE; 
          PACKAGE R IS END R;

          PACKAGE BODY R IS
          BEGIN
               IF CON'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR OBJECT " &
                             "OF IN MODE - 7" );
               END IF;

               IF IN_CON'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR OBJECT " &
                             "OF IN MODE - 8" );
               END IF;

               IF IN_UNC'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR OBJECT " &
                             "OF IN MODE - 9" );
               END IF;

               IF INOUT_CON'CONSTRAINED THEN
                    NULL;
               ELSE
                    FAILED ( "'CONSTRAINED IS 'FALSE' FOR " &
                             "CONSTRAINED OBJECT OF IN OUT MODE - 3" );
               END IF;

               IF INOUT_UNC'CONSTRAINED THEN
                    FAILED ( "'CONSTRAINED IS 'TRUE' FOR " &
                             "UNCONSTRAINED OBJECT OF IN OUT MODE " &
                             "- 3" );
               END IF;

          END R;

          PACKAGE S IS NEW R (SC, AC, BC, AU, BU);

     BEGIN
          P (SC, AC, BC, CC, AU, BU, CU);
          T.Q (SC, AC, BC, CC, AU, BU, CU);
     END;
          
     RESULT;
END C37402A;
