       *> { dg-do run }
       *> { dg-options "-fno-static-call -rdynamic" }
       *> { dg-output-file "group2/Static_CALL_with_ON_EXCEPTION__with_-fno-static-call_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      caller.
       PROCEDURE        DIVISION.
           CALL "callee1" ON EXCEPTION
              CALL "callee2" ON EXCEPTION
                  DISPLAY "neither callee1 nor callee2 found"
              END-CALL
           END-CALL
           GOBACK.
       END PROGRAM caller.
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      callee2.
       PROCEDURE        DIVISION.
           DISPLAY "this is callee2" NO ADVANCING
           GOBACK.

