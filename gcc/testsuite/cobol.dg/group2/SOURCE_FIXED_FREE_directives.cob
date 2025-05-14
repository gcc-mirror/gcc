       *> { dg-do run }
       *> { dg-output-file "group2/SOURCE_FIXED_FREE_directives.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       >>SOURCE FREE
   DATA             DIVISION.
   WORKING-STORAGE  SECTION.
   >>SOURCE FIXED
       PROCEDURE        DIVISION.                                       FIXED
             DISPLAY "OK" NO ADVANCING
             END-DISPLAY.
       >>SOURCE FREE
                                                                        DISPLAY
   "OK"
 NO ADVANCING
   END-DISPLAY.
      >>SOURCE FORMAT FIXED
             DISPLAY "OK" NO ADVANCING                                  FIXED
             END-DISPLAY.
       >>SOURCE FORMAT IS FREE
                                                                        DISPLAY
   "OK"
 NO ADVANCING
   END-DISPLAY.
             STOP RUN.

