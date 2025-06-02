-- C4A011A.ADA

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
-- CHECK THAT NONSTATIC UNIVERSAL REAL EXPRESSIONS ARE EVALUATED WITH
-- THE ACCURACY OF THE MOST PRECISE PREDEFINED FLOATING POINT TYPE
-- (I. E., THE TYPE FOR WHICH 'DIGITS EQUALS SYSTEM.MAX_DIGITS).

-- RJW 8/4/86

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C4A011A IS

     TYPE MAX_FLOAT IS DIGITS MAX_DIGITS;

     C5L : CONSTANT := 16#0.AAAA8#;
     C5U : CONSTANT := 16#0.AAAAC#;

     C6L : CONSTANT := 16#0.AAAAA8#;
     C6U : CONSTANT := 16#0.AAAAB0#;

     C7L : CONSTANT := 16#0.AAAAAA8#;
     C7U : CONSTANT := 16#0.AAAAAB0#;

     C8L : CONSTANT := 16#0.AAAAAAA#;
     C8U : CONSTANT := 16#0.AAAAAAB#;

     C9L : CONSTANT := 16#0.AAAAAAAA#;
     C9U : CONSTANT := 16#0.AAAAAAAC#;

     C10L : CONSTANT := 16#0.AAAAAAAAA#;
     C10U : CONSTANT := 16#0.AAAAAAAAC#;

     C11L : CONSTANT := 16#0.AAAAAAAAA8#;
     C11U : CONSTANT := 16#0.AAAAAAAAAC#;

     C12L : CONSTANT := 16#0.AAAAAAAAAA8#;
     C12U : CONSTANT := 16#0.AAAAAAAAAB0#;

     C13L : CONSTANT := 16#0.AAAAAAAAAAA8#;
     C13U : CONSTANT := 16#0.AAAAAAAAAAB0#;

     C14L : CONSTANT := 16#0.AAAAAAAAAAAA#;
     C14U : CONSTANT := 16#0.AAAAAAAAAAAB#;

     C15L : CONSTANT := 16#0.AAAAAAAAAAAAA#;
     C15U : CONSTANT := 16#0.AAAAAAAAAAAAC#;

     C16L : CONSTANT := 16#0.AAAAAAAAAAAAAA#;
     C16U : CONSTANT := 16#0.AAAAAAAAAAAAAC#;

     C17L : CONSTANT := 16#0.AAAAAAAAAAAAAA8#;
     C17U : CONSTANT := 16#0.AAAAAAAAAAAAAAC#;

     C18L : CONSTANT := 16#0.AAAAAAAAAAAAAAA8#;
     C18U : CONSTANT := 16#0.AAAAAAAAAAAAAAB0#;

     C19L : CONSTANT := 16#0.AAAAAAAAAAAAAAAA8#;
     C19U : CONSTANT := 16#0.AAAAAAAAAAAAAAAB0#;

     C20L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAA#;
     C20U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAB#;

     C21L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAA#;
     C21U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAC#;

     C22L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAA#;
     C22U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAC#;

     C23L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAA8#;
     C23U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAC#;

     C24L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAA8#;
     C24U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAB0#;

     C25L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAA8#;
     C25U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAB0#;

     C26L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAA#;
     C26U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAB#;

     C27L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAA#;
     C27U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAC#;

     C28L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAA#;
     C28U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAC#;

     C29L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAA8#;
     C29U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAC#;

     C30L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAA8#;
     C30U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAB0#;

     C31L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAAA#;
     C31U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAAB#;

     C32L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAAAA#;
     C32U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAAAB#;

     C33L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAAAAA#;
     C33U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAAAAC#;

     C34L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAAAAA8#;
     C34U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAAAAAC#;

     C35L : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAAAAAA8#;
     C35U : CONSTANT := 16#0.AAAAAAAAAAAAAAAAAAAAAAAAAAAAAC#;

BEGIN

     TEST ( "C4A011A", "CHECK THAT NONSTATIC UNIVERSAL REAL " & 
                       "EXPRESSIONS ARE EVALUATED WITH THE " &
                       "ACCURACY OF THE MOST PRECISE PREDEFINED " &
                       "FLOATING POINT TYPE (I. E., THE TYPE FOR " &
                       "WHICH 'DIGITS EQUALS SYSTEM.MAX_DIGITS" );

     CASE MAX_DIGITS IS
          WHEN 5 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C5L ..  C5U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 5" );
               END IF;
          WHEN 6 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C6L ..  C6U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 6" );
               END IF;
          WHEN 7 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C7L ..  C7U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 7" );
               END IF;
          WHEN 8 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C8L ..  C8U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 8" );
               END IF;
          WHEN 9 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C9L ..  C9U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 9" );
               END IF;
          WHEN 10 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C10L ..  C10U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 10" );
               END IF;
          WHEN 11 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C11L ..  C11U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 11" );
               END IF;
          WHEN 12 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C12L ..  C12U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 12" );
               END IF;
          WHEN 13 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C13L ..  C13U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 13" );
               END IF;
          WHEN 14 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C14L ..  C14U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 14" );
               END IF;
          WHEN 15 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C15L ..  C15U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 15" );
               END IF;
          WHEN 16 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C16L ..  C16U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 16" );
               END IF;
          WHEN 17 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C17L ..  C17U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 17" );
               END IF;
          WHEN 18 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C18L ..  C18U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 18" );
               END IF;
          WHEN 19 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C19L ..  C19U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 19" );
               END IF;
          WHEN 20 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C20L ..  C20U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 20" );
               END IF;
          WHEN 21 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C21L ..  C21U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 21" );
               END IF;
          WHEN 22 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C22L ..  C22U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 22" );
               END IF;
          WHEN 23 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C23L ..  C23U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 23" );
               END IF;
          WHEN 24 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C24L ..  C24U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 24" );
               END IF;
          WHEN 25 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C25L ..  C25U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 25" );
               END IF;
          WHEN 26 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C26L ..  C26U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 26" );
               END IF;
          WHEN 27 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C27L ..  C27U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 27" );
               END IF;
          WHEN 28 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C28L ..  C28U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 28" );
               END IF;
          WHEN 29 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C29L ..  C29U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 29" );
               END IF;
          WHEN 30 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C30L ..  C30U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 30" );
               END IF;
          WHEN 31 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C31L ..  C31U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 31" );
               END IF;
          WHEN 32 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C32L ..  C32U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 32" );
               END IF;
          WHEN 33 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C33L ..  C33U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 33" );
               END IF;
          WHEN 34 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C34L ..  C34U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 34" );
               END IF;
          WHEN 35 =>
               IF (2.0 * INTEGER'POS (IDENT_INT (1))) / 3.0 NOT IN
                  C35L ..  C35U THEN
                    FAILED ( "INCORRECT ACCURACY FOR A MAX_DIGITS " &
                             "VALUE OF 35" );
               END IF;
          WHEN OTHERS =>
               NOT_APPLICABLE ( "MAX_DIGITS OUT OF RANGE OF TEST.  " &
                                "MAX_DIGITS = " & 
                                 INTEGER'IMAGE (MAX_DIGITS));
     END CASE;

     RESULT;

END C4A011A;
