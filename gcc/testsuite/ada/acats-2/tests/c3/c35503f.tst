-- C35503F.TST

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
--     CHECK THAT 'IMAGE' AND 'VALUE' YIELD THE CORRECT RESULT FOR THE
--     LARGEST/SMALLEST INTEGER LITERAL AND A FORMAL DISCRETE TYPE WHOSE
--     ACTUAL PARAMETER IS AN INTEGER TYPE.

-- HISTORY
--     RJW 05/12/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C35503F IS

TYPE LONGEST_INT IS RANGE MIN_INT .. MAX_INT;

BEGIN
     TEST ("C35503F", "CHECK THAT 'IMAGE' AND 'VALUE' YIELD " &
                      "CORRECT RESULTS FOR THE LARGEST/SMALLEST "&
                      "INTEGER LITERAL AND A FORMAL DISCRETE TYPE " &
                      "WHOSE ACTUAL PARAMETER IS AN INTEGER TYPE");

     -- INTEGER_FIRST IS THE DECIMAL LITERAL IMAGE OF INTEGER'FIRST.
     -- INTEGER_LAST IS THE DECIMAL LITERAL IMAGE OF INTEGER'LAST.
     -- MIN_INT IS THE DECIMAL LITERAL IMAGE OF SYSTEM.MIN_INT.
     -- MAX_INT IS THE DECIMAL LITERAL IMAGE OF SYSTEM.MAX_INT.

     DECLARE
          GENERIC
               TYPE INT IS (<>);
          PROCEDURE P ( FS, LS : STRING; FI, LI : INT );

          PROCEDURE P ( FS, LS : STRING; FI, LI : INT ) IS
          BEGIN
               BEGIN
                    IF INT'VALUE (FS) /= FI THEN
                         FAILED ( "INCORRECT RESULTS FOR 'VALUE' OF " &
                                  FS );
                    END IF;
               EXCEPTION
                     WHEN CONSTRAINT_ERROR =>
                         FAILED ( "CONSTRAINT_ERROR RAISED FOR " &
                                  "'VALUE' OF " & FS );
                     WHEN OTHERS =>
                         FAILED ( "OTHER EXCEPTION RAISED FOR " &
                                  "'VALUE' OF " & FS );
               END;

               BEGIN
                    IF INT'VALUE (LS) /= LI THEN
                         FAILED ( "INCORRECT RESULTS FOR 'VALUE' OF " &
                                  LS );
                    END IF;
               EXCEPTION
                     WHEN CONSTRAINT_ERROR =>
                         FAILED ( "CONSTRAINT_ERROR RAISED FOR " &
                                  "'VALUE' OF " & LS );
                     WHEN OTHERS =>
                         FAILED ( "OTHER EXCEPTION RAISED FOR " &
                                  "'VALUE' OF " & LS );
               END;
          END P;

          GENERIC
               TYPE INT IS  (<>);
          PROCEDURE Q ( FS, LS : STRING; FI, LI : INT );

          PROCEDURE Q ( FS, LS : STRING; FI, LI : INT ) IS
          BEGIN
               BEGIN
                     IF INT'IMAGE(FI) /= FS THEN
                          FAILED ( "INCORRECT RESULTS FOR " &
                                   "'IMAGE' WITH " & FS );
                     END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED FOR 'IMAGE' " &
                                   "WITH " & FS );
               END;

               BEGIN
                     IF INT'IMAGE(LI) /= LS THEN
                         FAILED ( "INCORRECT RESULTS FOR " &
                                  "'IMAGE' WITH " & LS );
                     END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED FOR 'IMAGE' " &
                                  "WITH " & LS );
               END;
          END Q;

          PROCEDURE P1 IS NEW P ( INTEGER );
          PROCEDURE Q1 IS NEW Q ( INTEGER );
          PROCEDURE P2 IS NEW P ( LONGEST_INT );
          PROCEDURE Q2 IS NEW Q ( LONGEST_INT );
     BEGIN
          P1 ("$INTEGER_FIRST", "$INTEGER_LAST", INTEGER'FIRST,
                                                         INTEGER'LAST);
          P2 ("$MIN_INT", "$MAX_INT", MIN_INT, MAX_INT);
          Q1 ("$INTEGER_FIRST"," $INTEGER_LAST", INTEGER'FIRST,
                                                         INTEGER'LAST);
          Q2 ("$MIN_INT", " $MAX_INT", MIN_INT, MAX_INT);

     END;

     RESULT;
END C35503F;
