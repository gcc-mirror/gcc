       *> { dg-do run }
       *> { dg-output-file "group2/ROUNDED_TRUNCATION.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  M                PIC S9.
       01  N                PIC S9.
       01  O                PIC S9.
       01  P                PIC S9.
       01  Q                PIC S9.
       01  R                PIC S9.
       01  S                PIC S9.
       01  T                PIC S9.
       01  U                PIC S9.
       01  V                PIC S9.
       PROCEDURE DIVISION.
           COMPUTE M ROUNDED MODE TRUNCATION
                   = 2.49
           END-COMPUTE
           COMPUTE N ROUNDED MODE TRUNCATION
                   = -2.49
           END-COMPUTE
           COMPUTE O ROUNDED MODE TRUNCATION
                   = 2.50
           END-COMPUTE
           COMPUTE P ROUNDED MODE TRUNCATION
                   = -2.50
           END-COMPUTE
           COMPUTE Q ROUNDED MODE TRUNCATION
                   = 3.49
           END-COMPUTE
           COMPUTE R ROUNDED MODE TRUNCATION
                   = -3.49
           END-COMPUTE
           COMPUTE S ROUNDED MODE TRUNCATION
                   = 3.50
           END-COMPUTE
           COMPUTE T ROUNDED MODE TRUNCATION
                   = -3.50
           END-COMPUTE
           COMPUTE U ROUNDED MODE TRUNCATION
                   = 3.510
           END-COMPUTE
           COMPUTE V ROUNDED MODE TRUNCATION
                   = -3.510
           END-COMPUTE
           DISPLAY M " " N " " O " " P " " Q " " R " " S " " T
                   " " U " " V
               NO ADVANCING
           END-DISPLAY
           STOP RUN.

