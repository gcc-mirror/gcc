*> { dg-do compile }
*> { dg-options "-flto" { target lto } }
IDENTIFICATION DIVISION.
PROGRAM-ID. CobolGreeting.
*>Program to display COBOL greetings
DATA DIVISION.
WORKING-STORAGE SECTION.
01  IterNum   PIC 9 VALUE 5.

PROCEDURE DIVISION.
BeginProgram.
   PERFORM DisplayGreeting IterNum TIMES.
   STOP RUN.

DisplayGreeting.
   DISPLAY "Greetings from COBOL".
