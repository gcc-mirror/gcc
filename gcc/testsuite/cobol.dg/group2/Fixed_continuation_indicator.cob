       *> { dg-do run }
       *> { dg-output-file "group2/Fixed_continuation_indicator.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC X(333) VALUE
           '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWX
      -    'YZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUV
      -    'WXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRST
      -    'UVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQR
      -    'STUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOP
      -             'QRSTUVWXYZ'.
       PROCEDURE        DIVISION.
           DISPLAY X NO ADVANCING
           END-DISPLAY.
           DISPLAY '_'
           END-DISPLAY.
           MOVE
           "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567
      -    "89abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345
      -    "6789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123
      -    "456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01
      -     "23456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXY
      -                                                               "Z
      -             "0123456789" TO X.
           DISPLAY X NO ADVANCING
           END-DISPLAY.
           DISPLAY '_'
           END-DISPLAY.
           STOP RUN.

