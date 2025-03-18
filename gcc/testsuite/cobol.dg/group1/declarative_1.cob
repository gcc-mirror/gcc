*> { dg-do run }
*> { dg-output {Turning EC\-ALL CHECKING OFF \-\- Expecting \+00\.00 from ACOS\(\-3\)(\n|\r\n|\r)} }
*> { dg-output {      \+00\.00      TABL\(VSIX\) is 6(\n|\r\n|\r)} }
*> { dg-output {Turning EC\-ARGUMENT\-FUNCTION CHECKING ON(\n|\r\n|\r)} }
*> { dg-output {      Expecting \+0\.00 and DECLARATIVE FOR EC\-ARGUMENT\-FUNCTION(\n|\r\n|\r)} }
*> { dg-output {      DECLARATIVE FOR EC\-ARGUMENT\-FUNCTION(\n|\r\n|\r)} }
*> { dg-output {      \+00\.00      TABL\(VSIX\) is 6(\n|\r\n|\r)} }
*> { dg-output {Turning EC\-ARGUMENT CHECKING ON(\n|\r\n|\r)} }
*> { dg-output {      Expecting \+0\.00 and DECLARATIVE FOR EC\-ARGUMENT\-FUNCTION(\n|\r\n|\r)} }
*> { dg-output {      DECLARATIVE FOR EC\-ARGUMENT\-FUNCTION(\n|\r\n|\r)} }
*> { dg-output {      \+00\.00      TABL\(VSIX\) is 6(\n|\r\n|\r)} }
*> { dg-output {Turning EC\-ALL CHECKING ON(\n|\r\n|\r)} }
*> { dg-output {      Expecting \+0\.00 and DECLARATIVE EC\-ARGUMENT\-FUNCTION(\n|\r\n|\r)} }
*> { dg-output {      Followed by DECLARATIVE EC\-ALL for TABL\(6\) access(\n|\r\n|\r)} }
*> { dg-output {      DECLARATIVE FOR EC\-ARGUMENT\-FUNCTION(\n|\r\n|\r)} }
*> { dg-output {      \+00\.00      TABL\(VSIX\) is 1(\n|\r\n|\r)} }
*> { dg-output {      DECLARATIVE FOR EC\-ALL} }
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 VAL PIC S99V99.
        01 FILLER VALUE "1234567890".
           05 TABL  PIC X OCCURS 5.
           05 TABL2 PIC X OCCURS 5.
        01 VSIX   PIC 9 VALUE 6.
        PROCEDURE DIVISION.
        DECLARATIVES.
        DECLARATIVES-EC-ARGUMENT-FUNCTION SECTION.
            USE AFTER EXCEPTION CONDITION EC-ARGUMENT-FUNCTION.
                DISPLAY "      DECLARATIVE FOR EC-ARGUMENT-FUNCTION".
                RESUME NEXT STATEMENT.
        DECLARATIVES-EC-ARGUMENT SECTION.
            USE AFTER EXCEPTION CONDITION EC-ARGUMENT.
                DISPLAY "      DECLARATIVE FOR EC-ARGUMENT".
                RESUME NEXT STATEMENT.
        DECLARATIVES-EC-ALL SECTION.
            USE AFTER EXCEPTION CONDITION EC-ALL.
                DISPLAY "      DECLARATIVE FOR EC-ALL".
                RESUME NEXT STATEMENT.
           END DECLARATIVES.
      *> END DECLARATIVES must be followed by an explicit section.
      *> See ISO 2014 section 14.2.1
      *> READ ISO 2023 section 14.2.1 Format 2 (without sections) and
      *> you will note that they forgot to isolate the declaratives from
      *> the rest of the PROCEDURE DIVISION. So NO an explicit section
      *> IS NOT REQUIRED.
      *> See below that the >>TURN-EC-ALL CHECKING OFF statements at the end
      *> of paragraphs are commented out.  As of this writing, GCOBOL improperly
      *> treats that as a syntax error.  This is a known problem.
        MAIN-SECTION SECTION.
        PERFORM TEST1.
        PERFORM TEST2.
        PERFORM TEST3.
        PERFORM TEST4.
      *>  PERFORM TEST5
        GOBACK.
        TEST1.
        DISPLAY "Turning EC-ALL CHECKING OFF -- Expecting +00.00 from ACOS(-3)"
        >>TURN EC-ALL CHECKING OFF
      *> The assumption that ACOS should return an invalid response is
      *> in violation of the definition of ACOS in the standard. Furthermore,
      *> EC-ARGUMENT-FUNCTION is marked FATAL and elsewhere in the standard
      *> it says the implementor has the option to continue (scary) or fail.
      *> By fail I think that means perform the declarative and then, if
      *> the declarative section does not issue a RESUME ... "the run unit is
      *> terminated abnormally as specified in 14.6.12, Abnormal run unit
      *> termination." Not a segfault, ever. Jim mentioned he was looking for
      *> a solution for RESUME but terminating as specified is not a
      *> segfault.
        MOVE FUNCTION ACOS(-3) TO VAL.
        DISPLAY "      " VAL WITH NO ADVANCING.
        DISPLAY "      TABL(VSIX) is " TABL(VSIX).
      *>  >>TURN EC-ALL CHECKING OFF
        TEST2.
        >>TURN EC-ALL CHECKING OFF
        DISPLAY "Turning EC-ARGUMENT-FUNCTION CHECKING ON"
        DISPLAY "      " "Expecting +0.00 and DECLARATIVE FOR EC-ARGUMENT-FUNCTION"
        >>TURN EC-ARGUMENT-FUNCTION CHECKING ON
        MOVE FUNCTION ACOS(-3) TO VAL.
        DISPLAY "      " VAL WITH NO ADVANCING.
        DISPLAY "      TABL(VSIX) is " TABL(VSIX).
      *>  >>TURN EC-ALL CHECKING OFF
        TEST3.
        >>TURN EC-ALL CHECKING OFF
        DISPLAY "Turning EC-ARGUMENT CHECKING ON"
        DISPLAY "      " "Expecting +0.00 and DECLARATIVE FOR EC-ARGUMENT-FUNCTION"
        >>TURN EC-ARGUMENT CHECKING ON
      *> Since there is a declarative for EC-ARGUMENT-FUNCTION, per Jim
      *> that section will be used in this case and the higher-level
      *> exception section will not. If that has changed, then the notion
      *> of hierarchic response is different than we agreed.
        MOVE FUNCTION ACOS(-3) TO VAL.
        DISPLAY "      " VAL WITH NO ADVANCING.
        DISPLAY "      TABL(VSIX) is " TABL(VSIX).
      *>  >>TURN EC-ALL CHECKING OFF
        TEST4.
        >>TURN EC-ALL CHECKING OFF
      *> Same as previous.
        DISPLAY "Turning EC-ALL CHECKING ON"
        DISPLAY "      " "Expecting +0.00 and DECLARATIVE EC-ARGUMENT-FUNCTION"
        DISPLAY "      " "Followed by DECLARATIVE EC-ALL for TABL(6) access"
        >>TURN EC-ALL CHECKING ON
        MOVE FUNCTION ACOS(-3) TO VAL.
        DISPLAY "      " VAL WITH NO ADVANCING.
        DISPLAY "      TABL(VSIX) is " TABL(VSIX).
      *>  >>TURN EC-ALL CHECKING OFF
        TEST5.
        >>TURN EC-ALL CHECKING OFF
        DISPLAY "Turning EC-BOUND-SUBSCRIPT CHECKING ON - expecting default termination"
        >>TURN EC-BOUND-SUBSCRIPT CHECKING ON
        MOVE FUNCTION ACOS(-3) TO VAL.
        DISPLAY "      " VAL WITH NO ADVANCING.
        DISPLAY "      TABL(VSIX) is " TABL(VSIX).
      *>  >>TURN EC-ALL CHECKING OFF
        END PROGRAM prog.
