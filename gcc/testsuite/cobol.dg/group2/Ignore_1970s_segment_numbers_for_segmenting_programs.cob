      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-Wno-segment -dialect ibm" }
       *> { dg-output-file "group2/Ignore_1970s_segment_numbers_for_segmenting_programs.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. PROG.
        PROCEDURE DIVISION.
        INIT SECTION 01.
        para-01.
           DISPLAY "OK" NO ADVANCING.
           EXIT.
        NITI SECTION 02.
        para-02.
           DISPLAY "KO" NO ADVANCING.
           EXIT.
           GOBACK.

