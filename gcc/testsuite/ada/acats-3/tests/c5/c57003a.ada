-- C57003A.ADA

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
-- CHECK THAT THE EXIT STATEMENT IS EVALUATED EACH TIME THROUGH A LOOP,
--    AND THAT IT IS EVALUATED CORRECTLY WHETHER POSITIONED AT THE
--    BEGINNING, MIDDLE, OR END OF THE LOOP.



-- EACH TEST IS A LOOP ON  J  WHERE THE EXIT CONDITIONS ARE TO EVALUATE
--    TO  'FALSE'  A CERTAIN NUMBER OF TIMES UNTIL, AT THE APPROPRIATE
--    TIME, ONE OF THEM EVALUATES TO  'TRUE'  AND CAUSES THE LOOP TO BE
--    EXITED.
--
--
--    THE TEST IS PERFORMED 30 TIMES FOR EACH OF THE FIRST TWO
--    DATA TYPES CONSIDERED ('INTEGER', USER-DEFINED ENUMERATION)
--    AND 26 TIMES FOR 'CHARACTER' (THUS 86 TIMES ALTOGETHER).
--
--
--    EACH DATA TYPE HAS ITS OWN SEPARATE SECTION OF CODE.  ALL SECTIONS
--    FOLLOW THE SAME TESTING ALGORITHM (MUTATIS MUTANDIS).  THE CALCU-
--    LATIONS WHICH KEEP TRACK OF THE FLOW OF CONTROL ARE ALL DONE IN
--    INTEGER ARITHMETIC.  THERE ARE THREE DATA TYPES, THUS THREE
--    SECTIONS.
--
--
--    FOR EACH DATA TYPE, THE 30 TESTS ARE DIVIDED INTO 3 "SEGMENTS"
--
--               << NOTE:  THE NUMBER OF SEGMENTS IS WRITTEN  " 3 "    ,
--                         THE NUMBER OF SECTIONS IS WRITTEN  "THREE" >>
--
--    (OF 10 TESTS EACH, EXCEPT 10,10,6 FOR 'CHARACTER'), NUMBERED
--     0 , 1 , 2  AND CORRESPONDING TO THE 3 SIGNIFICANTLY DIFFERENT
--    POSITIONS OF AN EXIT STATEMENT WITH RESPECT TO THE LOOP IT IS IN
--    ( "AT THE VERY TOP" , "AT THE VERY BOTTOM" , "ANYWHERE IN BETWEEN"
--    ).  AT THE BEGINNING OF EACH TEST, THE VARIABLE  WHICH_SEGMENT
--    IS UPDATED TO CONTAIN THE NEW VALUE OF THIS IDENTIFYING NUMBER
--    (FOR THE TEST ABOUT TO BEGIN):
--
--             EXIT AT THE TOP      ........   WHICH_SEGMENT = 0
--             EXIT FROM THE MIDDLE ........   WHICH_SEGMENT = 1
--             EXIT AT THE BOTTOM   ........   WHICH_SEGMENT = 2   .
--
--
--    WITHIN EACH SECTION, THE TESTS ARE NUMBERED  FROM  1  TO  30
--    (26 FOR 'CHARACTER').  THIS NUMBER IS STORED IN THE INTEGER
--    VARIABLE  INT_I  (EQUAL TO THE CURRENT VALUE OF THE OUTER-LOOP
--    INDEX WHEN THAT INDEX IS OF INTEGER TYPE), WHOSE APPROPRIATE VALUE
--    FOR EACH TEST IS SET AT THE BEGINNING OF THE TEST.
--
--
--    AS PART OF THE EVALUATION PROCESS, THE PROGRAM COMPUTES FOR EACH
--    TEST (I.E.  FOR EACH VALUE OF  I , OR OF  INT_I ) THE APPROPRIATE
--    NUMBER OF INNER-LOOP ITERATIONS REQUIRED BEFORE EXIT; THIS IS
--    THE EXPECTED VALUE OF  J  (EXPRESSED AS AN INTEGER IN THE RANGE
--     1..10 ) AND STORES IT IN  EXPECTED_J .  FOR EACH OF THE THREE
--    SECTIONS, THE TIME SEQUENCE OF THESE 30 VALUES IS
--
--             1   2   3   4   5   6   7   8   9  10     << SEGMENT 1 >>
--             6   6   7   7   8   8   9   9  10  10     << SEGMENT 2 >>
--             7   8   8   8   9   9   9  10  10  10     << SEGMENT 3 >>
--
--    (EACH SECTION GETS ALL 3 ROWS, NOT ONE ROW PER SECTION;
--    FOR 'CHARACTER', WHERE ONLY 26 VALUES ARE REQUIRED, THE LAST 4
--    VALUES ARE OMITTED).  THIS NUMBER IS COMPARED WITH THE ACTUAL
--    VALUE OF  J  (ACTUAL NUMBER OF INNER-LOOP ITERATIONS BEFORE THE
--    EXECUTION OF THE EXIT STATEMENT) AS SAVED JUST BEFORE THE EXIT
--    FROM THE LOOP (AGAIN IN THE FORM OF AN INTEGER IN THE RANGE
--     1..30 , IRRESPECTIVE OF THE DATA TYPE BEING TESTED),  I F
--    SUCH SAVED VALUE IS AVAILABLE.
--
--
--    THE ACTUAL VALUE OF INNER-LOOP ITERATIONS (AS SAVED IMMEDIATELY
--    BEFORE THE EXIT, AS OPPOSED TO A VALUE LEFT OVER FROM SOME
--    PREVIOUS ITERATION) IS AVAILABLE ONLY IF  WHICH_SEGMENT /= 0 ,
--    AND IS THEN STORED IN  SAVE_J .
--
--
--    FOR THE CASE  WHICH_SEGMENT = 0 , THE ITERATIONS ARE COUNTED IN
--    THE VARIABLE  COUNT , WHOSE VALUE AT THE COMPLETION OF THE
--    I-TH TEST ( I IN 1..10 ) MUST BE EQUAL TO  EXPECTED_J - 1 ,
--    AND THUS TO  I - 1  (METHODOLOGICALLY AS WELL AS COMPUTATIONALLY
--    THIS IS NO DIFFERENT FROM USING THE MOST RECENT VALUE OF  SAVE_J
--    WHEN A CURRENT ONE CANNOT BE OBTAINED).  AFTER BEING INCREMENTED
--    BY  1 ,  COUNT  IS CHECKED AGAINST  EXPECTED_J .
--
--
--    THIS CONCLUDES THE DESCRIPTION OF THE CASE  WHICH_SEGMENT = 0 ,
--    AND THUS OF THE ALGORITHM.  THE ONLY REASON FOR SPLITTING THE
--    CASE  WHICH_SEGMENT /= 0  INTO TWO IS THE DESIRE TO PROVIDE FOR
--    DISTINCT MESSAGES.



-- RM 04/23/81
-- SPS 3/7/83

WITH REPORT;
PROCEDURE  C57003A  IS

     USE  REPORT ;

BEGIN

     TEST( "C57003A" , "TEST THAT THE EXIT STATEMENT IS EVALUATED" &
                       " EACH TIME THROUGH THE LOOP"               );

     DECLARE

          WHICH_SEGMENT  :  INTEGER RANGE 0..2   ;   -- BOUNDS ARE TIGHT
          SAVE_J         :  INTEGER RANGE 1..10  ;
          EXPECTED_J     :  INTEGER RANGE 1..10  ;
          COUNT          :  INTEGER RANGE 0..100     := 0 ;
          INT_I          :  INTEGER RANGE 1..30  ;

          TYPE  ENUM  IS    ( CHANGE_THE_ORIGIN_FROM_0_TO_1 ,

               A1 , A2 , A3 , A4 , A5 , A6 , A7 , A8 , A9 , A10 ,
               A11, A12, A13, A14, A15, A16, A17, A18, A19, A20 ,
               A21, A22, A23, A24, A25, A26, A27, A28, A29, A30 );

     BEGIN


          --------------------------------------------------------------
          -----------------------  INTEGER  ----------------------------


          FOR  I  IN  INTEGER RANGE 1..30  LOOP


               WHICH_SEGMENT  :=  ( I - 1 ) / 10        ;
               EXPECTED_J     :=  ( I + WHICH_SEGMENT ) /
                                  ( WHICH_SEGMENT + 1 ) ;

               COUNT  :=  0 ;


               FOR  J  IN  INTEGER RANGE 1..10  LOOP

                    --  J  NOT SAVED HERE (SO THAT 'EXIT' BE FIRST STMT)

                    EXIT WHEN  WHICH_SEGMENT = 0  AND
                               1*J >= I ;--COUNT+:=1 ON NXT LINE INSTEAD
                    COUNT := COUNT + 1 ;

                    NULL ;
                    NULL ;
                    NULL ;
                    SAVE_J := J ;
                    EXIT WHEN  WHICH_SEGMENT = 1  AND
                               2*J >= I ;

                    NULL ;
                    NULL ;
                    NULL ;
                    SAVE_J := J ;
                    EXIT WHEN  WHICH_SEGMENT = 2  AND
                               3*J >= I ;

               END LOOP;


               COUNT          :=  COUNT + 1             ;  -- SEE HEADER

               CASE  WHICH_SEGMENT  IS
                    WHEN  0  =>
                         IF  COUNT  /=  EXPECTED_J  THEN
                              FAILED( "WRONG COUNT; INT, EXIT AT TOP" );
                         END IF;
                    WHEN  1  =>                 -- WOULD WORK ALSO FOR 0
                         IF  SAVE_J  /=  EXPECTED_J  THEN
                              FAILED( "WRONG COUNT; I,EXIT AT MIDDLE" );
                         END IF;
                    WHEN  2  =>
                         IF  SAVE_J  /=  EXPECTED_J  THEN
                              FAILED( "WRONG COUNT; I,EXIT AT BOTTOM" );
                         END IF;
               END CASE;

          END LOOP;



          --------------------------------------------------------------
          ----------------------  CHARACTER  ---------------------------


          FOR  I  IN  CHARACTER RANGE 'A'..'Z'  LOOP

               INT_I  := CHARACTER'POS(I) - CHARACTER'POS('A') + 1;

               WHICH_SEGMENT  :=  ( INT_I - 1 ) / 10        ;
               EXPECTED_J     :=  ( INT_I + WHICH_SEGMENT ) /
                                  ( WHICH_SEGMENT + 1 )     ;

               COUNT  :=  0 ;


               FOR  J  IN  CHARACTER RANGE 'A'..'J'  LOOP

                    --  J  NOT SAVED HERE (SO THAT 'EXIT' BE FIRST STMT)

                    EXIT WHEN  WHICH_SEGMENT = 0  AND
                               J >= I ; -- COUNT+:=1 ON NXT LINE INSTEAD
                    COUNT := COUNT + 1 ;

                    NULL ;
                    NULL ;
                    NULL ;
                    SAVE_J := CHARACTER'POS(J) - CHARACTER'POS('A') + 1;
                    EXIT WHEN  WHICH_SEGMENT = 1  AND
                               2 * SAVE_J >= INT_I ;

                    NULL ;
                    NULL ;
                    NULL ;
                    EXIT WHEN  WHICH_SEGMENT = 2  AND
                               3 * SAVE_J >= INT_I ;

               END LOOP;


               COUNT          :=    COUNT + 1               ;

               CASE  WHICH_SEGMENT  IS
                    WHEN  0  =>
                         IF  COUNT  /=  EXPECTED_J  THEN
                              FAILED( "WRONG COUNT;CHAR, EXIT AT TOP" );
                         END IF;
                    WHEN  1  =>                 -- WOULD WORK ALSO FOR 0
                         IF  SAVE_J  /=  EXPECTED_J  THEN
                              FAILED( "WRONG COUNT; C,EXIT AT MIDDLE" );
                         END IF;
                    WHEN  2  =>
                         IF  SAVE_J  /=  EXPECTED_J  THEN
                              FAILED( "WRONG COUNT; C,EXIT AT BOTTOM" );
                         END IF;
               END CASE;

          END LOOP;



          --------------------------------------------------------------
          ---------------------  ENUMERATION  --------------------------


          FOR  I  IN  ENUM RANGE A1..A30  LOOP


               INT_I          :=    ENUM'POS(I) ;

               WHICH_SEGMENT  :=  ( INT_I - 1 ) / 10        ;
               EXPECTED_J     :=  ( INT_I + WHICH_SEGMENT ) /
                                  ( WHICH_SEGMENT + 1 )     ;

               COUNT  :=  0 ;


               FOR  J  IN  ENUM RANGE A1..A10  LOOP

                    --  J  NOT SAVED HERE (SO THAT 'EXIT' BE FIRST STMT)

                    EXIT WHEN  WHICH_SEGMENT = 0  AND
                               J >= I ; -- COUNT+:=1 ON NXT LINE INSTEAD
                    COUNT := COUNT + 1 ;

                    NULL ;
                    NULL ;
                    NULL ;
                    SAVE_J := ENUM'POS(J) ;
                    EXIT WHEN  WHICH_SEGMENT = 1  AND
                               2 * SAVE_J >= INT_I ;

                    NULL ;
                    NULL ;
                    NULL ;
                    EXIT WHEN  WHICH_SEGMENT = 2  AND
                               3 * SAVE_J >= INT_I ;

               END LOOP;


               COUNT          :=    COUNT + 1               ;

               CASE  WHICH_SEGMENT  IS
                    WHEN  0  =>
                         IF  COUNT  /=  EXPECTED_J  THEN
                              FAILED( "WRONG COUNT;ENUM, EXIT AT TOP" );
                         END IF;
                    WHEN  1  =>                 -- WOULD WORK ALSO FOR 0
                         IF  SAVE_J  /=  EXPECTED_J  THEN
                              FAILED( "WRONG COUNT; E,EXIT AT MIDDLE" );
                         END IF;
                    WHEN  2  =>
                         IF  SAVE_J  /=  EXPECTED_J  THEN
                              FAILED( "WRONG COUNT; E,EXIT AT BOTTOM" );
                         END IF;
               END CASE;

          END LOOP;

          --------------------------------------------------------------

     END ;


     RESULT ;


END  C57003A ;
