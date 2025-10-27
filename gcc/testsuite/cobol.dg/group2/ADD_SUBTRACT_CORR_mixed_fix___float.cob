       *> { dg-do run }
       *> { dg-output-file "group2/ADD_SUBTRACT_CORR_mixed_fix___float.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 GROUP1.
            05 VAR1 PIC 9999    VALUE 1.
            05 VAR2 PIC 9999    VALUE 2.
            05 VAR3     COMP-2  VALUE 3.
            05 VAR4     COMP-2  VALUE 4.
        01 GROUP2.
            05 VAR1 PIC 9999    VALUE 1000.
            05 VAR2     COMP-2  VALUE 2000.
            05 VAR3 PIC 9999    VALUE 3000.
            05 VAR4     COMP-2  VALUE 4000.
        PROCEDURE DIVISION.
            PERFORM DISP2
            ADD CORRESPONDING GROUP1 TO GROUP2
            PERFORM DISP2
            SUBTRACT CORRESPONDING GROUP1 FROM GROUP2
            PERFORM DISP2.
            GOBACK.
        DISP2.
            DISPLAY
                    VAR1 OF GROUP2 SPACE
                    VAR2 OF GROUP2 SPACE
                    VAR3 OF GROUP2 SPACE
                    VAR4 OF GROUP2.
        END PROGRAM prog.


