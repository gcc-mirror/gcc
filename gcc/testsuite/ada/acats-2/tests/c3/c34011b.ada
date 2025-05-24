-- C34011B.ADA

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
--     CHECK THAT A DERIVED TYPE DECLARATION IS NOT CONSIDERED EXACTLY
--     EQUIVALENT TO AN ANONYMOUS DECLARATION OF THE DERIVED TYPE
--     FOLLOWED BY A SUBTYPE DECLARATION OF THE DERIVED SUBTYPE.  IN
--     PARTICULAR, CHECK THAT CONSTRAINT_ERROR CAN BE RAISED WHEN THE
--     SUBTYPE INDICATION OF THE DERIVED TYPE DECLARATION IS ELABORATED
--     (EVEN THOUGH THE CONSTRAINT WOULD SATISFY THE DERIVED (BASE)
--     TYPE).

-- HISTORY:
--     JRK 09/04/87  CREATED ORIGINAL TEST.
--     EDS 07/29/98  AVOID OPTIMIZATION

WITH REPORT; USE REPORT;

PROCEDURE C34011B IS

     SUBTYPE BOOL IS BOOLEAN RANGE FALSE .. FALSE;

     SUBTYPE FLT IS FLOAT RANGE -10.0 .. 10.0;

     SUBTYPE DUR IS DURATION RANGE 0.0 .. 10.0;

     SUBTYPE INT IS INTEGER RANGE 0 .. 10;

     TYPE ARR IS ARRAY (INT RANGE <>) OF INTEGER;

     TYPE REC (D : INT := 0) IS
          RECORD
               I : INTEGER;
          END RECORD;

     PACKAGE PT IS
          TYPE PRIV (D : POSITIVE := 1) IS PRIVATE;
     PRIVATE
          TYPE PRIV (D : POSITIVE := 1) IS
               RECORD
                    I : INTEGER;
               END RECORD;
     END PT;

     USE PT;

     TYPE ACC_ARR IS ACCESS ARR;

     TYPE ACC_REC IS ACCESS REC;

BEGIN
     TEST ("C34011B", "CHECK THAT CONSTRAINT_ERROR CAN BE RAISED " &
                      "WHEN THE SUBTYPE INDICATION OF A DERIVED TYPE " &
                      "DECLARATION IS ELABORATED");

     BEGIN
          DECLARE
            TYPE T IS NEW BOOL RANGE FALSE .. BOOL(IDENT_BOOL(TRUE));

          BEGIN
            DECLARE
               -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
               T1 : T := T(IDENT_BOOL(TRUE));
            BEGIN
               FAILED ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
            EXCEPTION
               WHEN OTHERS =>
                     FAILED ("DID NOT RAISE CONSTRAINT_ERROR" &
                             " AT PROPER PLACE - BOOL " & 
                             T'IMAGE(T1) );   --USE T1);
            END;

            FAILED ("EXCEPTION NOT RAISED - BOOL");

          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("WRONG HANDLER ENTERED - BOOL");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - BOOL");
     END;

     BEGIN
          DECLARE
            TYPE T IS NEW POSITIVE RANGE IDENT_INT (0) .. 10;

          BEGIN
            DECLARE
               -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
               T1 : T := T(IDENT_INT(1));
            BEGIN
                 FAILED ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
            EXCEPTION
               WHEN OTHERS =>
                     FAILED ("DID NOT RAISE CONSTRAINT_ERROR - POSITIVE " &
                             T'IMAGE(T1)); --USE T1
            END;
            FAILED ("EXCEPTION NOT RAISED - POSITIVE" );
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("WRONG HANDLER ENTERED - POSITIVE");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - POSITIVE");
     END;

     BEGIN
          DECLARE
            TYPE T IS NEW FLT RANGE 0.0 .. FLT(IDENT_INT(20));

          BEGIN
            DECLARE
               -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
               T1 : T := T(IDENT_INT(0));
            BEGIN
               FAILED ("DID NOT RAISE CONSTRAINT_ERROR" &
                       " AT PROPER PLACE " &
                       T'IMAGE(T1) ); --USE T1

            EXCEPTION
               WHEN OTHERS =>
                     FAILED ("DID NOT RAISE CONSTRAINT_ERROR" &
                             " AT PROPER PLACE "); 
            END;
            FAILED ("EXCEPTION NOT RAISED - FLT" );
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("WRONG HANDLER ENTERED - FLT");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - FLT");
     END;

     BEGIN
          DECLARE
            TYPE T IS NEW DUR RANGE DUR(IDENT_INT(-1)) .. 5.0;


          BEGIN
            DECLARE
               -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
               T1 : T := T(IDENT_INT(2));
            BEGIN
               FAILED ("DID NOT RAISE CONSTRAINT_ERROR" &
                       " AT PROPER PLACE " &
                       T'IMAGE(T1) );  -- USE T1
            EXCEPTION
               WHEN OTHERS =>
                     FAILED ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
            END;
            FAILED ("EXCEPTION NOT RAISED - DUR " );
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("WRONG HANDLER ENTERED - DUR");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - DUR");
     END;

     BEGIN
          DECLARE
            TYPE T IS NEW ARR (IDENT_INT (-1) .. 10);

          BEGIN
            DECLARE
               -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
               T1 : T := (OTHERS => IDENT_INT(3));
            BEGIN
               FAILED ("DID NOT RAISE CONSTRAINT_ERROR " &
                       "AT PROPER PLACE " &
                       INTEGER'IMAGE(T1(1)) ); --USE T1
            EXCEPTION
               WHEN OTHERS =>
                     FAILED ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
            END;
            FAILED ("EXCEPTION NOT RAISED - ARR " );
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("WRONG HANDLER ENTERED - ARR");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - ARR");
     END;

     BEGIN
          DECLARE
               TYPE T IS NEW REC (IDENT_INT (11));

          BEGIN
            DECLARE
               -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
               T1 : T;
            BEGIN
               FAILED ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
            EXCEPTION
               WHEN OTHERS =>
                     FAILED ("DID NOT RAISE CONSTRAINT_ERROR " &
                             "AT PROPER PLACE " &
                             INTEGER'IMAGE(T1.D) ); --USE T1
            END;
            FAILED ("EXCEPTION NOT RAISED - REC " );
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("WRONG HANDLER ENTERED - REC");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - REC");
     END;

     BEGIN
          DECLARE
               TYPE T IS NEW PRIV (IDENT_INT (0));  --RAISES C_E

          BEGIN
            DECLARE
               -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
               T1 : T;
            BEGIN
               FAILED ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
            EXCEPTION
               WHEN OTHERS =>
                     FAILED ("DID NOT RAISE CONSTRAINT_ERROR " & 
                             "AT PROPER PLACE " &
                             INTEGER'IMAGE(T1.D) ); --USE T1
            END;
            FAILED ("EXCEPTION NOT RAISED - PRIV " ); 
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("WRONG HANDLER ENTERED - PRIV");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - PRIV");
     END;

     BEGIN
          DECLARE
            TYPE T IS NEW ACC_ARR (0 .. IDENT_INT (11));  --RAISES C_E

          BEGIN
            DECLARE
               -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
               T1 : T;
            BEGIN
               FAILED ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
            EXCEPTION
               WHEN OTHERS =>
                     FAILED ("DID NOT RAISE CONSTRAINT_ERROR " &
                             "AT PROPER PLACE " &
                             INTEGER'IMAGE(T1(1)) ); --USE T1
            END;
            FAILED ("EXCEPTION NOT RAISED - ACC_ARR " );
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("WRONG HANDLER ENTERED - ACC_ARR");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - ACC_ARR");
     END;

     BEGIN
          DECLARE
               TYPE T IS NEW ACC_REC (IDENT_INT (-1));  --RAISES C_E

          BEGIN
            DECLARE
               -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
               T1 : T;
            BEGIN
               FAILED ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
            EXCEPTION
               WHEN OTHERS =>
                     FAILED ("DID NOT RAISE CONSTRAINT_ERROR " & 
                             "AT PROPER PLACE " &
                             INTEGER'IMAGE(T1.D) ); --USE T1
            END;
                FAILED ("EXCEPTION NOT RAISED - ACC_REC " );
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("WRONG HANDLER ENTERED - ACC_REC");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - ACC_REC");
     END;

     RESULT;
END C34011B;
