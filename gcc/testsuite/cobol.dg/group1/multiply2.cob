*> { dg-do run }
*> { dg-output {Test of MULTIPLY\. All results should be 20(\n|\r\n|\r)} }
*> { dg-output {TEST01\-1 20 20 20(\n|\r\n|\r)} }
*> { dg-output {TEST01\-2 20 20 20(\n|\r\n|\r)} }
*> { dg-output {TEST02\-1 20(\n|\r\n|\r)} }
*> { dg-output {TEST02\-2 20(\n|\r\n|\r)} }
*> { dg-output {TEST02\-3 20(\n|\r\n|\r)} }
*> { dg-output {TEST02\-4 20(\n|\r\n|\r)} }
*> { dg-output {TEST02\-5 20 20 20(\n|\r\n|\r)} }
*> { dg-output {TEST02\-6 20 20 20(\n|\r\n|\r)} }
*> { dg-output {TEST02\-7 20 20 20(\n|\r\n|\r)} }
*> { dg-output {TEST02\-8 20 20 20(\n|\r\n|\r)} }
*> { dg-output {Thank you for running the MULTIPLY test\.} }
        IDENTIFICATION DIVISION.
        PROGRAM-ID.  mult.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  A         PIC 9  VALUE 4.
        01  B         PIC 9  VALUE 5.
        01  X       PIC 99 VALUE ZEROS.
        01  Y       PIC 99 VALUE ZEROS.
        01  Z       PIC 99 VALUE ZEROS.
        PROCEDURE DIVISION.
            DISPLAY "Test of MULTIPLY. All results should be 20"
            *> Two cases of FORMAT 1
            PERFORM SET5.
            MULTIPLY 4 BY X Y Z.
            DISPLAY "TEST01-1 " X " " Y " " Z
            PERFORM SET5.
            MULTIPLY A BY X Y Z.
            DISPLAY "TEST01-2 " X " " Y " " Z.
            *> Eight cases of FORMAT2 2
            PERFORM CLEAR
            MULTIPLY 4 BY 5 GIVING X
            DISPLAY "TEST02-1 " X
            PERFORM CLEAR
            MULTIPLY A BY 5 GIVING X
            DISPLAY "TEST02-2 " X
            PERFORM CLEAR
            MULTIPLY 4 BY B GIVING X
            DISPLAY "TEST02-3 " X
            PERFORM CLEAR
            MULTIPLY A BY B GIVING X
            DISPLAY "TEST02-4 " X
            PERFORM CLEAR
            MULTIPLY 4 BY 5 GIVING X Y Z
            DISPLAY "TEST02-5 " X " " Y " " Z
            PERFORM CLEAR
            MULTIPLY A BY 5 GIVING X Y Z
            DISPLAY "TEST02-6 " X " " Y " " Z
            PERFORM CLEAR
            MULTIPLY 4 BY B GIVING X Y Z
            DISPLAY "TEST02-7 " X " " Y " " Z
            PERFORM CLEAR
            MULTIPLY A BY B GIVING X Y Z
            DISPLAY "TEST02-8 " X " " Y " " Z
            DISPLAY "Thank you for running the MULTIPLY test."
            STOP RUN.
        CLEAR.
            MOVE 0 TO X
            MOVE 0 TO Y
            MOVE 0 TO Z.
        SET5.
            MOVE 5 TO X
            MOVE 5 TO Y
            MOVE 5 TO Z.
        LAST-PARAGRAPH.
            END PROGRAM mult.
