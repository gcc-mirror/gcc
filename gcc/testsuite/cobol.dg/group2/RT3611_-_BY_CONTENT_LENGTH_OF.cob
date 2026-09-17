      *> Do not edit this generated file.  See README.txt
      *> { dg-do compile }
       *> { dg-options "-dialect ibm" }
[
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(30).

       PROCEDURE DIVISION.
           CALL "TEST01"
                USING BY CONTENT LENGTH OF WS-NAME
           GOBACK.
       END PROGRAM prog.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST01.
       PROCEDURE DIVISION.
       DISPLAY "OK" NO ADVANCING.
       GOBACK.
       END PROGRAM TEST01.
]
