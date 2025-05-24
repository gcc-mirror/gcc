-- CC3019C0.ADA

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
-- OBJECTIVE
--   THIS IS GENERIC PACKAGE WHICH IS USED TO CHECK THE LEVEL OF
--   NESTED GENERICS SUPPORTED BY AN IMPLEMENTATION.
--
-- HISTORY:
--         EDWARD V. BERARD, 31 AUGUST 1990

GENERIC

     TYPE ELEMENT IS LIMITED PRIVATE ;

     WITH PROCEDURE ASSIGN (SOURCE        : IN OUT ELEMENT ;
                            DESTINATION   : IN OUT ELEMENT) ;

     WITH FUNCTION "=" (LEFT  : IN ELEMENT ;
                        RIGHT : IN ELEMENT) RETURN BOOLEAN ;

PACKAGE CC3019C0_LIST_CLASS IS

     TYPE LIST IS LIMITED PRIVATE ;

     OVERFLOW    : EXCEPTION ;
     UNDERFLOW   : EXCEPTION ;

     PROCEDURE ADD    (THIS_ELEMENT        : IN OUT ELEMENT ;
                       TO_THIS_LIST        : IN OUT LIST) ;

     PROCEDURE DELETE (THIS_ELEMENT      : IN OUT ELEMENT ;
                       FROM_THIS_LIST    : IN OUT LIST) ;

     PROCEDURE COPY   (THIS_LIST           : IN OUT LIST ;
                       TO_THIS_LIST        : IN OUT LIST) ;

     PROCEDURE CLEAR  (THIS_LIST           : IN OUT LIST) ;

     GENERIC

          WITH PROCEDURE PROCESS (THIS_ELEMENT    : IN  ELEMENT ;
                                  CONTINUE        : OUT BOOLEAN) ;

     PROCEDURE ITERATE (OVER_THIS_LIST    : IN LIST) ;

     FUNCTION NUMBER_OF_ELEMENTS (IN_THIS_LIST : IN LIST)
          RETURN NATURAL ;

     FUNCTION "=" (LEFT  : IN LIST ;
                   RIGHT : IN LIST) RETURN BOOLEAN ;

PRIVATE

     TYPE LIST_TABLE IS ARRAY (POSITIVE RANGE 1 .. 10) OF ELEMENT ;

     TYPE LIST IS RECORD
          LENGTH        : NATURAL := 0 ;
          ACTUAL_LIST   : LIST_TABLE ;
     END RECORD ;

END CC3019C0_LIST_CLASS ;

PACKAGE BODY CC3019C0_LIST_CLASS IS

     PROCEDURE ADD    (THIS_ELEMENT        : IN OUT ELEMENT ;
                       TO_THIS_LIST        : IN OUT LIST) IS

     BEGIN  -- ADD

          IF TO_THIS_LIST.LENGTH >= 10 THEN
               RAISE OVERFLOW ;
          ELSE
               TO_THIS_LIST.LENGTH := TO_THIS_LIST.LENGTH + 1 ;
               ASSIGN (
                    SOURCE      => THIS_ELEMENT,
                    DESTINATION =>
                         TO_THIS_LIST.ACTUAL_LIST(TO_THIS_LIST.LENGTH));
          END IF ;

     END ADD ;

     PROCEDURE DELETE (THIS_ELEMENT      : IN OUT ELEMENT ;
                       FROM_THIS_LIST    : IN OUT LIST) IS

     BEGIN  -- DELETE

          IF FROM_THIS_LIST.LENGTH <= 0 THEN
               RAISE UNDERFLOW ;
          ELSE
               ASSIGN (
                    SOURCE      =>
                      FROM_THIS_LIST.ACTUAL_LIST(FROM_THIS_LIST.LENGTH),
                    DESTINATION => THIS_ELEMENT) ;
               FROM_THIS_LIST.LENGTH := FROM_THIS_LIST.LENGTH - 1 ;
          END IF ;

     END DELETE ;

     PROCEDURE COPY   (THIS_LIST           : IN OUT LIST ;
                       TO_THIS_LIST        : IN OUT LIST) IS

     BEGIN  -- COPY

          TO_THIS_LIST.LENGTH := THIS_LIST.LENGTH ;
          FOR INDEX IN TO_THIS_LIST.ACTUAL_LIST'RANGE LOOP
               ASSIGN (SOURCE      => THIS_LIST.ACTUAL_LIST (INDEX),
                       DESTINATION => TO_THIS_LIST.ACTUAL_LIST (INDEX));
          END LOOP ;

     END COPY ;

     PROCEDURE CLEAR  (THIS_LIST         : IN OUT LIST) IS

     BEGIN  -- CLEAR

          THIS_LIST.LENGTH := 0 ;

     END CLEAR ;

     PROCEDURE ITERATE (OVER_THIS_LIST    : IN LIST) IS

          CONTINUE : BOOLEAN := TRUE ;
          FINISHED : NATURAL := 0 ;

     BEGIN  -- ITERATE

          WHILE (CONTINUE = TRUE) AND (FINISHED < OVER_THIS_LIST.LENGTH)
               LOOP
                    FINISHED := FINISHED + 1 ;
                    PROCESS (THIS_ELEMENT =>
                                OVER_THIS_LIST.ACTUAL_LIST (FINISHED),
                             CONTINUE     => CONTINUE) ;
               END LOOP ;

     END ITERATE ;

     FUNCTION NUMBER_OF_ELEMENTS (IN_THIS_LIST : IN LIST)
          RETURN NATURAL IS

     BEGIN  -- NUMBER_OF_ELEMENTS

          RETURN IN_THIS_LIST.LENGTH ;

     END NUMBER_OF_ELEMENTS ;

     FUNCTION "=" (LEFT  : IN LIST ;
                   RIGHT : IN LIST) RETURN BOOLEAN IS

          RESULT : BOOLEAN := TRUE ;
          INDEX  : NATURAL := 0 ;

     BEGIN  -- "="

          IF LEFT.LENGTH /= RIGHT.LENGTH THEN
               RESULT := FALSE ;
          ELSE
               WHILE (INDEX < LEFT.LENGTH) AND RESULT LOOP
                    INDEX := INDEX + 1 ;
                    IF LEFT.ACTUAL_LIST (INDEX) /=
                       RIGHT.ACTUAL_LIST (INDEX) THEN
                        RESULT := FALSE ;
                    END IF ;
               END LOOP ;
          END IF ;

          RETURN RESULT ;

     END "=" ;

END CC3019C0_LIST_CLASS ;
