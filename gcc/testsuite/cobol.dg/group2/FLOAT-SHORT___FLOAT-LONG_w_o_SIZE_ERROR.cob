       *> { dg-do run }
       *> { dg-output-file "group2/FLOAT-SHORT___FLOAT-LONG_w_o_SIZE_ERROR.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CMP1                        COMP-1.
       01  SV1                         COMP-1.
       01  CMP2                        COMP-2.
       01  SV2                         COMP-2.

       PROCEDURE DIVISION.
       CND-000.

           DISPLAY "--- COMP-1 ---"
           COMPUTE CMP1 = (((1.0E7 / 2.1E0) / 3.1E0) - 5.0E-1) * 6.0E0
           DISPLAY "A: " CMP1
           COMPUTE CMP1 = (((1.0E7 / 2.9E0) / 3.9E0) - 5.0E-1) * 6.0E0
           DISPLAY "B: " CMP1
           MOVE ZERO TO CMP1.
           COMPUTE CMP1 = 1.0E3 / 2.1E0
                   ON SIZE ERROR DISPLAY "Z: " CMP1 " SIZE ERROR"
               NOT ON SIZE ERROR DISPLAY "Z: " CMP1 " IS OK"
           END-COMPUTE.

           DISPLAY "    ..."
           DISPLAY "--- COMP-2 ---"
           COMPUTE CMP2 = (((1.0E7 / 2.1E0) / 3.1E0) - 5.0E-1) * 6.0E0
      *>   because of possible rounding of intermediates and different
      *>   precision depending on math library / version: plain DISPLAY
           IF CMP2 >= 9216586.86175114 AND <= 9216586.86175116
             DISPLAY "A ~ 9216586.86175115"
           ELSE
             DISPLAY "A: " CMP2
           END-IF
           COMPUTE CMP2 = (((1.0E7 / 2.9E0) / 3.9E0) - 5.0E-1) * 6.0E0
           IF CMP2 >= 5305036.7877983 AND <= 5305036.7877985
             DISPLAY "B ~ 5305036.787798408"
           ELSE
             DISPLAY "B: " CMP2
           END-IF
           MOVE ZERO TO CMP2.
           COMPUTE CMP2 = 1.0E3 / 2.1E0
                   ON SIZE ERROR DISPLAY "Z: " CMP2 " SIZE ERROR"
               NOT ON SIZE ERROR
      *>        see note above
                IF CMP2 >= 476.1904761904760 AND <= 476.1904761904763
                  DISPLAY "Z ~ 476.1904761904761 IS OK"
                ELSE
                  DISPLAY "Z: " CMP2 " IS OK"
                END-IF
           END-COMPUTE.

           DISPLAY "    ..."
           DISPLAY "--- 99 + 1 / 3 ---"
           MOVE -1 TO CMP1, CMP2.
           COMPUTE CMP1 = 99 + 1 / 3
                   ON SIZE ERROR DISPLAY "CMP1: " CMP1 " SIZE ERROR"
               NOT ON SIZE ERROR DISPLAY "CMP1: " CMP1 " IS OK"
           END-COMPUTE.
           COMPUTE CMP2 = 99 + 1 / 3
                   ON SIZE ERROR DISPLAY "CMP2: " CMP2 " SIZE ERROR"
               NOT ON SIZE ERROR DISPLAY "CMP2: " CMP2 " IS OK"
           END-COMPUTE.

           DISPLAY "    ..."
           DISPLAY "--- 99 ---"
           MOVE -1 TO CMP1, CMP2.
           COMPUTE CMP1 = 99
                   ON SIZE ERROR DISPLAY "CMP1: " CMP1 " SIZE ERROR"
               NOT ON SIZE ERROR DISPLAY "CMP1: " CMP1 " IS OK"
           END-COMPUTE.
           COMPUTE CMP2 = 99
                   ON SIZE ERROR DISPLAY "CMP2: " CMP2 " SIZE ERROR"
               NOT ON SIZE ERROR DISPLAY "CMP2: " CMP2 " IS OK"
           END-COMPUTE.

       CND-100-OK.
           DISPLAY "    ..."
           DISPLAY "--- Test overflow ---"

           MOVE 990000 TO CMP1.
           PERFORM 6500 TIMES
             MOVE CMP1 TO SV1
             COMPUTE CMP1 = CMP1 * 10
                    ON SIZE ERROR GO TO CND-350-ERR
             END-COMPUTE
             IF CMP1 < 9.0
               GO TO CND-350-ERR
             END-IF
           END-PERFORM.
           DISPLAY "CMP1: " CMP1 " IS OK".
           GO TO CND-350-OK.
       CND-350-ERR.
           DISPLAY "CMP1: after " SV1 " SIZE ERROR".

       CND-350-OK.
           MOVE 9900000000 TO CMP2.
           PERFORM 6500 TIMES
             MOVE CMP2 TO SV2
             COMPUTE CMP2 = CMP2 * 10
                    ON SIZE ERROR GO TO CND-380-ERR
             END-COMPUTE
             IF CMP2 < 9.0
               GO TO CND-380-ERR
             END-IF
           END-PERFORM.
           DISPLAY "CMP2: " CMP2 " IS OK".
           GO TO CND-500-OK.
       CND-380-ERR.
      *>   because of possible rounding of intermediates and different
      *>   precision depending on math library / version: plain DISPLAY
           IF SV2 >= 9.899999999999E+307 AND
                  <= 9.900000000001E+307
             DISPLAY "CMP2: after ~ 9.899999999999781E+307 SIZE ERROR"
           ELSE
             DISPLAY "CMP2: after " SV2 " SIZE ERROR"
           END-IF
           .

       CND-500-OK.
           MOVE 0.000000099 TO CMP1.
           PERFORM 350 TIMES
             MOVE CMP1 TO SV1
             COMPUTE CMP1 = CMP1 / 10.0
                    ON SIZE ERROR GO TO CND-500-ERR
             END-COMPUTE
             IF CMP1 = 0.0
               GO TO CND-500-ERR
             END-IF
           END-PERFORM.
           DISPLAY "CMP1: " CMP1 " IS OK".
           GO TO CND-600-OK.
       CND-500-ERR.
           DISPLAY "CMP1: after " SV1 " SIZE ERROR".

       CND-600-OK.
           MOVE 0.000000099 TO CMP2.
           PERFORM 350 TIMES
             MOVE CMP2 TO SV2
             COMPUTE CMP2 = CMP2 / 10.0
                    ON SIZE ERROR GO TO CND-600-ERR
             END-COMPUTE
             IF CMP2 = 0.0
               GO TO CND-600-ERR
             END-IF
           END-PERFORM.
           DISPLAY "CMP2: " CMP2 " IS OK".
           GO TO CND-600-XIT.
       CND-600-ERR.
           IF SV2 >= 9.8813129168249E-324 AND <= 9.881312916825E-324
             DISPLAY "CMP2: after ~ 9.881312916824931E-324 SIZE ERROR"
           ELSE
             DISPLAY "CMP2: after " SV2 " SIZE ERROR"
           END-IF
           .
       CND-600-XIT.

       CND-999.
           STOP RUN.
       END PROGRAM prog.

