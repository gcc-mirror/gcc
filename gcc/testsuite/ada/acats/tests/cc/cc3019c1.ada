-- CC3019C1.ADA

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
--  THIS IS GENERIC PACKAGE WHICH IS USED TO CHECK THE LEVEL OF
--  NESTED GENERICS SUPPORTED BY AN IMPLEMENTATION. IT IS USED
--  BY MAIN PROCEDURE CC3019C2M.ADA.
--
-- HISTORY:
--         EDWARD V. BERARD, 31 AUGUST 1990

WITH CC3019C0_LIST_CLASS ;

GENERIC

     TYPE ELEMENT IS LIMITED PRIVATE ;

     WITH PROCEDURE ASSIGN (SOURCE        : IN OUT ELEMENT ;
                            DESTINATION   : IN OUT ELEMENT) ;

     WITH FUNCTION "=" (LEFT  : IN ELEMENT ;
                        RIGHT : IN ELEMENT) RETURN BOOLEAN ;

PACKAGE CC3019C1_NESTED_GENERICS IS

     TYPE NESTED_GENERICS_TYPE IS LIMITED PRIVATE ;

     PROCEDURE COPY (SOURCE        : IN OUT NESTED_GENERICS_TYPE ;
                     DESTINATION   : IN OUT NESTED_GENERICS_TYPE) ;

     PROCEDURE SET_ELEMENT
                    (FOR_THIS_NGT_OBJECT : IN OUT NESTED_GENERICS_TYPE ;
                     TO_THIS_ELEMENT     : IN OUT ELEMENT) ;

     PROCEDURE SET_NUMBER
                    (FOR_THIS_NGT_OBJECT : IN OUT NESTED_GENERICS_TYPE ;
                     TO_THIS_NUMBER      : IN NATURAL) ;

     FUNCTION "=" (LEFT  : IN NESTED_GENERICS_TYPE ;
                   RIGHT : IN NESTED_GENERICS_TYPE) RETURN BOOLEAN ;

     FUNCTION ELEMENT_OF (THIS_NGT_OBJECT    : IN NESTED_GENERICS_TYPE)
          RETURN ELEMENT ;

     FUNCTION NUMBER_OF  (THIS_NGT_OBJECT    : IN NESTED_GENERICS_TYPE)
          RETURN NATURAL ;

     GENERIC

          TYPE ELEMENT IS LIMITED PRIVATE ;

          WITH PROCEDURE ASSIGN (SOURCE        : IN OUT ELEMENT ;
                                 DESTINATION   : IN OUT ELEMENT) ;

     PACKAGE GENERIC_TASK IS

          TASK TYPE PROTECTED_AREA IS

                    ENTRY STORE (ITEM    : IN OUT ELEMENT) ;
                    ENTRY GET   (ITEM    : IN OUT ELEMENT) ;

          END PROTECTED_AREA ;

     END GENERIC_TASK ;

     GENERIC

          TYPE ELEMENT IS LIMITED PRIVATE ;

          WITH PROCEDURE ASSIGN (SOURCE        : IN OUT ELEMENT ;
                                 DESTINATION   : IN OUT ELEMENT) ;

          WITH FUNCTION "=" (LEFT  : IN ELEMENT ;
                             RIGHT : IN ELEMENT) RETURN BOOLEAN ;

     PACKAGE STACK_CLASS IS

          TYPE STACK IS LIMITED PRIVATE ;

          OVERFLOW    : EXCEPTION ;
          UNDERFLOW   : EXCEPTION ;

          PROCEDURE PUSH (THIS_ELEMENT        : IN OUT ELEMENT ;
                          ON_TO_THIS_STACK    : IN OUT STACK) ;

          PROCEDURE POP  (THIS_ELEMENT        : IN OUT ELEMENT ;
                          OFF_THIS_STACK      : IN OUT STACK) ;

          PROCEDURE COPY  (THIS_STACK        : IN OUT STACK ;
                           TO_THIS_STACK    : IN OUT STACK) ;

          PROCEDURE CLEAR (THIS_STACK        : IN OUT STACK) ;

          GENERIC

               WITH PROCEDURE PROCESS (THIS_ELEMENT    : IN  ELEMENT ;
                                       CONTINUE        : OUT BOOLEAN) ;

          PROCEDURE ITERATE (OVER_THIS_STACK    : IN STACK) ;

          FUNCTION NUMBER_OF_ELEMENTS (ON_THIS_STACK    : IN STACK)
                    RETURN NATURAL ;

          FUNCTION "=" (LEFT  : IN STACK ;
                        RIGHT : IN STACK) RETURN BOOLEAN ;

     PRIVATE

          PACKAGE NEW_LIST_CLASS IS NEW
               CC3019C0_LIST_CLASS (ELEMENT => ELEMENT,
                                    ASSIGN  => ASSIGN,
                                    "="     => "=") ;

          TYPE STACK IS NEW NEW_LIST_CLASS.LIST ;

     END STACK_CLASS ;

PRIVATE

     TYPE NESTED_GENERICS_TYPE IS RECORD
          FIRST    : ELEMENT ;
          SECOND   : NATURAL ;
     END RECORD ;

END CC3019C1_NESTED_GENERICS ;

PACKAGE BODY CC3019C1_NESTED_GENERICS IS

     PROCEDURE COPY (SOURCE        : IN OUT NESTED_GENERICS_TYPE ;
                     DESTINATION   : IN OUT NESTED_GENERICS_TYPE) IS

     BEGIN  -- COPY

          ASSIGN (SOURCE        => SOURCE.FIRST,
                  DESTINATION   => DESTINATION.FIRST) ;

          DESTINATION.SECOND := SOURCE.SECOND ;

     END COPY ;

     PROCEDURE SET_ELEMENT
          (FOR_THIS_NGT_OBJECT : IN OUT NESTED_GENERICS_TYPE ;
          TO_THIS_ELEMENT     : IN OUT ELEMENT) IS

     BEGIN  -- SET_ELEMENT

          ASSIGN (SOURCE        => TO_THIS_ELEMENT,
                  DESTINATION   => FOR_THIS_NGT_OBJECT.FIRST) ;

     END SET_ELEMENT ;

     PROCEDURE SET_NUMBER
          (FOR_THIS_NGT_OBJECT : IN OUT NESTED_GENERICS_TYPE ;
          TO_THIS_NUMBER      : IN NATURAL) IS

     BEGIN  -- SET_NUMBER

          FOR_THIS_NGT_OBJECT.SECOND := TO_THIS_NUMBER ;

     END SET_NUMBER ;

     FUNCTION "=" (LEFT  : IN NESTED_GENERICS_TYPE ;
                   RIGHT : IN NESTED_GENERICS_TYPE) RETURN BOOLEAN IS

     BEGIN  -- "="

          IF (LEFT.FIRST = RIGHT.FIRST) AND
             (LEFT.SECOND = RIGHT.SECOND) THEN
                       RETURN TRUE ;
          ELSE
                    RETURN FALSE ;
          END IF ;

     END "=" ;

     FUNCTION ELEMENT_OF (THIS_NGT_OBJECT    : IN NESTED_GENERICS_TYPE)
          RETURN ELEMENT IS

     BEGIN  -- ELEMENT_OF

          RETURN THIS_NGT_OBJECT.FIRST ;

     END ELEMENT_OF ;

     FUNCTION NUMBER_OF (THIS_NGT_OBJECT    : IN NESTED_GENERICS_TYPE)
          RETURN NATURAL IS

     BEGIN  -- NUMBER_OF

          RETURN THIS_NGT_OBJECT.SECOND ;

     END NUMBER_OF ;

     PACKAGE BODY GENERIC_TASK IS

          TASK BODY PROTECTED_AREA IS

               LOCAL_STORE : ELEMENT ;

          BEGIN  -- PROTECTED_AREA

               LOOP
                    SELECT
                         ACCEPT STORE (ITEM    : IN OUT ELEMENT) DO
                              ASSIGN (SOURCE        => ITEM,
                                      DESTINATION   => LOCAL_STORE) ;
                         END STORE ;
                    OR
                         ACCEPT GET   (ITEM    : IN OUT ELEMENT) DO
                              ASSIGN (SOURCE        => LOCAL_STORE,
                                      DESTINATION   => ITEM) ;
                         END GET ;
                    OR
                         TERMINATE ;
                    END SELECT ;
               END LOOP ;

          END PROTECTED_AREA ;

     END GENERIC_TASK ;

     PACKAGE BODY STACK_CLASS IS

          PROCEDURE PUSH (THIS_ELEMENT        : IN OUT ELEMENT ;
                          ON_TO_THIS_STACK    : IN OUT STACK) IS

          BEGIN  -- PUSH

              NEW_LIST_CLASS.ADD (
                    THIS_ELEMENT    => THIS_ELEMENT,
                    TO_THIS_LIST    =>
                         NEW_LIST_CLASS.LIST (ON_TO_THIS_STACK)) ;

          EXCEPTION

              WHEN NEW_LIST_CLASS.OVERFLOW => RAISE OVERFLOW ;

          END PUSH ;

          PROCEDURE POP  (THIS_ELEMENT        : IN OUT ELEMENT ;
                          OFF_THIS_STACK      : IN OUT STACK) IS

          BEGIN  -- POP

               NEW_LIST_CLASS.DELETE (
                    THIS_ELEMENT     => THIS_ELEMENT,
                    FROM_THIS_LIST   =>
                        NEW_LIST_CLASS.LIST (OFF_THIS_STACK)) ;

          EXCEPTION

                    WHEN NEW_LIST_CLASS.UNDERFLOW => RAISE UNDERFLOW ;

          END POP ;

          PROCEDURE COPY  (THIS_STACK       : IN OUT STACK ;
                           TO_THIS_STACK    : IN OUT STACK) IS

          BEGIN  -- COPY

              NEW_LIST_CLASS.COPY (
                    THIS_LIST    => NEW_LIST_CLASS.LIST (THIS_STACK),
                    TO_THIS_LIST =>
                         NEW_LIST_CLASS.LIST (TO_THIS_STACK)) ;

          END COPY ;

          PROCEDURE CLEAR (THIS_STACK        : IN OUT STACK) IS

          BEGIN  -- CLEAR

               NEW_LIST_CLASS.CLEAR (NEW_LIST_CLASS.LIST (THIS_STACK)) ;

          END CLEAR ;

          PROCEDURE ITERATE (OVER_THIS_STACK  : IN STACK) IS

               PROCEDURE STACK_ITERATE IS NEW NEW_LIST_CLASS.ITERATE
                                        (PROCESS => PROCESS) ;

          BEGIN  -- ITERATE

               STACK_ITERATE (NEW_LIST_CLASS.LIST (OVER_THIS_STACK)) ;

          END ITERATE ;

          FUNCTION NUMBER_OF_ELEMENTS (ON_THIS_STACK    : IN STACK)
                    RETURN NATURAL IS

          BEGIN  -- NUMBER_OF_ELEMENTS

               RETURN NEW_LIST_CLASS.NUMBER_OF_ELEMENTS
                    (IN_THIS_LIST =>
                         NEW_LIST_CLASS.LIST (ON_THIS_STACK)) ;

          END NUMBER_OF_ELEMENTS ;

          FUNCTION "=" (LEFT  : IN STACK ;
                        RIGHT : IN STACK) RETURN BOOLEAN IS

          BEGIN  -- "="

               RETURN NEW_LIST_CLASS."=" (
                    LEFT  => NEW_LIST_CLASS.LIST (LEFT),
                   RIGHT => NEW_LIST_CLASS.LIST (RIGHT)) ;

          END "=" ;

     END STACK_CLASS ;

END CC3019C1_NESTED_GENERICS ;
