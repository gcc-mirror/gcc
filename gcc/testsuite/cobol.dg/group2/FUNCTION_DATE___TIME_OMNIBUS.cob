       *> { dg-do run }
       *> { dg-set-target-env-var TZ UTC0 }

        identification division.
        program-id. test.
      *>  Tests all the DATE and TIME functions
      *>
      *>  The various functions are used to test each other.
      *>
      *>  COMBINED-DATETIME             OK
      *>  CURRENT_DATE                  OK
      *>  DATE-OF-INTEGER               OK
      *>  DATE-TO-YYYYMMDD              OK
      *>  DAY-OF-INTEGER                OK
      *>  DAY-TO-YYYYDDD                OK
      *>  FORMATTED-CURRENT-DATE        OK
      *>  FORMATTED-DATE                OK
      *>  FORMATTED-DATETIME            OK
      *>  FORMATTED-TIME                OK
      *>  INTEGER-OF-DATE               OK
      *>  INTEGER-OF-DAY                OK
      *>  INTEGER-OF-FORMATTED-DATE     OK
      *>  SECONDS-FROM-FORMATTED-TIME   OK
      *>  SECONDS-PAST-MIDNIGHT         OK
      *>  TEST-DATE-YYYYMMDD            OK
      *>  TEST-DAY-YYYYDDD              OK
      *>  TEST-FORMATTED-DATETIME       OK
      *>
        data division.
        working-storage section.

        01 checking  pic x(80).
        01 should-be pic x(32).
        01 but-is    pic x(32).
        01 but-is-n redefines but-is pic 99999999.999999.
        01 but-is-integer_part pic 99999.

        01 jd1601 pic 9(7).
        01 jd9999 pic 9(7).
        01 jd     pic s9(7).

        01 integer-date pic s9(7).
        01 integer-result pic 99.
        01 standard-date-form pic 9(8).
        01 julian-date-form PIC 9(8).

        01 date1.
            02 YYYY pic 9999.
            02 MM     pic 99.
            02 DD     pic 99.
        01 date2.
            02 YYYY   pic 9999.
            02 filler pic x value "-".
            02 MM     pic 99.
            02 filler pic x value "-".
            02 DD     pic 99.
        01 date3.
            02 YYYY   pic 9999.
            02 DDD    pic 999.
        01 date4.
            02 YYYY   pic 9999.
            02 filler pic x value "-".
            02 DDD     pic 999.
        01 date5.
            02 YYYY   pic 9999.
            02 filler pic x value "W".
            02 ww     pic 99.
            02 d      pic 9.
        01 date6.
            02 YYYY   pic 9999.
            02 filler pic xx value "-W".
            02 ww     pic 99.
            02 filler pic x value "-".
            02 d      pic 9.

        01 yymmdd.
            02 YY pic 99.
            02 MM pic 99.
            02 DD pic 99.

        01 minus10 pic s99 value -10.

        01 forced_date_n pic X(64) VALUE Z"GCOBOL_CURRENT_DATE".
        01 forced_date_v pic X(64) VALUE Z"1945/06/01 12:34:56".

        procedure division.
        CALL "setenv" using forced_date_n, forced_date_v

        move "SECONDS-PAST-MIDNIGHT" to checking
        move "45296" to should-be
        MOVE FUNCTION SECONDS-PAST-MIDNIGHT to but-is-integer_part
        move but-is-integer_part to but-is
        perform checkit

      *>    Establish the initial date integer
        move "integer-of-date" to checking
        move function integer-of-date(19000101) to jd1601
        move "integer-of-date(19000101)" to checking
        move 0109208 to should-be
        move jd1601 to but-is
        perform checkit

      *>    Establish the final date integer
        move "integer-of-date" to checking
        move function integer-of-date(21011231) to jd9999
        move "integer-of-date(21001231)" to checking
        move 0182986 to should-be
        move jd9999 to but-is
        perform checkit

      *>    We are going to do the following tests over all valid dates:
        perform varying jd from jd1601 by 1 until jd > jd9999

      *>    Convert JD to all six DATE types:
            move FUNCTION FORMATTED-DATE("YYYYMMDD"   jd) TO date1
            move FUNCTION FORMATTED-DATE("YYYY-MM-DD" jd) TO date2
            move FUNCTION FORMATTED-DATE("YYYYDDD"    jd) TO date3
            move FUNCTION FORMATTED-DATE("YYYY-DDD"   jd) TO date4
            move FUNCTION FORMATTED-DATE("YYYYWwwD"   jd) TO date5
            move FUNCTION FORMATTED-DATE("YYYY-Www-D" jd) TO date6

      *>    Test the routines that check DATE types
            move zero to should-be
            move FUNCTION TEST-FORMATTED-DATETIME("YYYYMMDD"   date1) TO but-is
              move "TEST-FORMATTED-DATETIME(""YYYYMMDD""   date1)" to checking
              perform checkit
            move FUNCTION TEST-FORMATTED-DATETIME("YYYY-MM-DD" date2) TO but-is
              move "TEST-FORMATTED-DATETIME(""YYYY-MM-DD"" date2)" to checking
              perform checkit
            move FUNCTION TEST-FORMATTED-DATETIME("YYYYDDD"    date3) TO but-is
              move "TEST-FORMATTED-DATETIME(""YYYYDDD""    date3)" to checking
              perform checkit
            move FUNCTION TEST-FORMATTED-DATETIME("YYYY-DDD"   date4) TO but-is
              move "TEST-FORMATTED-DATETIME(""YYYY-DDD""   date4)" to checking
              perform checkit
            move FUNCTION TEST-FORMATTED-DATETIME("YYYYWwwD"   date5) TO but-is
              move "TEST-FORMATTED-DATETIME(""YYYYWwwD""   date5)" to checking
              perform checkit
            move FUNCTION TEST-FORMATTED-DATETIME("YYYY-Www-D" date6) TO but-is
              move "TEST-FORMATTED-DATETIME(""YYYY-Www-D"" date6)" to checking
              perform checkit

      *>    Test the routines that extract the integer date

            move function INTEGER-OF-FORMATTED-DATE("YYYYMMDD"   date1) TO integer-date
              move "INTEGER-OF-FORMATTED-DATE(""YYYYMMDD""   date1)" to checking
              move jd to should-be
              move integer-date to but-is
              perform checkit

            move function INTEGER-OF-FORMATTED-DATE("YYYY-MM-DD"   date2) TO integer-date
              move "INTEGER-OF-FORMATTED-DATE(""YYYY-MM-DD""   date2)" to checking
              move jd to should-be
              move integer-date to but-is
              perform checkit

            move function INTEGER-OF-FORMATTED-DATE("YYYYDDD"   date3) TO integer-date
              move "INTEGER-OF-FORMATTED-DATE(""YYYYDDD""   date3)" to checking
              move jd to should-be
              move integer-date to but-is
              perform checkit

            move function INTEGER-OF-FORMATTED-DATE("YYYY-DDD"   date4) TO integer-date
              move "INTEGER-OF-FORMATTED-DATE(""YYYY-DDD""   date4)" to checking
              move jd to should-be
              move integer-date to but-is
              perform checkit

            move function INTEGER-OF-FORMATTED-DATE("YYYYWwwD"   date5) TO integer-date
              move "INTEGER-OF-FORMATTED-DATE(""YYYYWwwD""   date5)" to checking
              move jd to should-be
              move integer-date to but-is
              perform checkit

            move function INTEGER-OF-FORMATTED-DATE("YYYY-Www-D"   date6) TO integer-date
              move "INTEGER-OF-FORMATTED-DATE(""YYYY-Www-D""   date6)" to checking
              move jd to should-be
              move integer-date to but-is
              perform checkit

            move function DATE-OF-INTEGER(jd) to standard-date-form
            move function INTEGER-OF-DATE(standard-date-form) to integer-date
              move "DATE-OF-INTEGER and INTEGER-OF-DATE" to checking
              move jd to should-be
              move integer-date to but-is
              perform checkit

            move function TEST-DATE-YYYYMMDD(standard-date-form) to integer-result
              move "TEST-DATE-YYYYMMDD" to checking
              move zero to should-be
              move integer-result to but-is
              perform checkit

            move function DAY-OF-INTEGER(jd) to julian-date-form
            move function INTEGER-OF-DAY(julian-date-form) to integer-date
              move "DAY-OF-INTEGER and INTEGER-OF-DAY" to checking
              move jd to should-be
              move integer-date to but-is
              perform checkit

            move function TEST-DAY-YYYYDDD(julian-date-form) to integer-result
              move "TEST-DAY-YYYYDDD" to checking
              move zero to should-be
              move integer-result to but-is
              perform checkit
            end-perform.

        move function integer-of-date(19980101) to jd1601
        move function integer-of-date(19981231) to jd9999
        perform varying jd from jd1601 by 1 until jd > jd9999
            move FUNCTION FORMATTED-DATE("YYYYMMDD"   jd) TO date1
            move FUNCTION FORMATTED-DATE("YYYYDDD"    jd) TO date3

            move FUNCTION MOD( YYYY of date1 100) to yy of yymmdd
            move MM of date1 to MM of yymmdd
            move DD of date1 to DD of yymmdd

            move FUNCTION DATE-TO-YYYYMMDD(yymmdd, minus10, 1994)
                        to standard-date-form
              move "DATE-TO-YYYYMMDD" to checking
              move "18" to date1(1:2)
              move date1 to should-be
              move standard-date-form to but-is
              perform checkit
            end-perform.

        move "DAY-TO-YYYYDDD" to checking
        MOVE 1910004 to should-be
        MOVE FUNCTION DAY-TO-YYYYDDD(10004 -20 2002) TO but-is
        perform checkit
        MOVE 1810004 to should-be
        MOVE FUNCTION DAY-TO-YYYYDDD(10004 -120 2002) TO but-is
        perform checkit
        MOVE 2010004 to should-be
        MOVE FUNCTION DAY-TO-YYYYDDD(10004 20 2002) TO but-is
        perform checkit
        MOVE 1995005 to should-be
        MOVE FUNCTION DAY-TO-YYYYDDD(95005 -10 2013) TO but-is
        perform checkit

        move "COMBINED-DATETIME" to checking
        MOVE "19450601.123456" TO should-be
        MOVE FUNCTION COMBINED-DATETIME(19450601 123456) TO but-is-n
        perform checkit

        move "CURRENT_DATE" to checking
        MOVE "1945060112345600+0000" TO should-be
        MOVE FUNCTION CURRENT-DATE TO but-is
        move "+0000" to but-is(17:5)
        perform checkit

        move "FORMATTED-CURRENT-DATE (1)" to checking
        MOVE "1945-06-01T12:34:56" TO should-be
        MOVE FUNCTION FORMATTED-CURRENT-DATE("YYYY-MM-DDThh:mm:ss") TO but-is
        perform blot-zulu
        perform checkit

        move "FORMATTED-CURRENT-DATE (2)" to checking
        MOVE "1945-06-01T12:34:56Z" TO should-be
        MOVE FUNCTION FORMATTED-CURRENT-DATE("YYYY-MM-DDThh:mm:ssZ") TO but-is
        perform blot-zulu
        perform checkit

        move "FORMATTED-CURRENT-DATE (3)" to checking
        MOVE "1945-06-01T12:34:56-05:00" TO should-be
        MOVE FUNCTION FORMATTED-CURRENT-DATE("YYYY-MM-DDThh:mm:ss+hh:mm") TO but-is
        perform blot-zulu
        perform checkit

        move "formatted-time" to checking
        move "01:12:34Z" to should-be
        MOVE FUNCTION formatted-time("hh:mm:ssZ" 754 -60 ) to but-is
        perform checkit.

        move "00:12:34Z" to should-be
        MOVE FUNCTION formatted-time("hh:mm:ssZ" 754   0 ) to but-is
        perform checkit.

        move "23:12:34Z" to should-be
        MOVE FUNCTION formatted-time("hh:mm:ssZ" 754  60 ) to but-is
        perform checkit.

        move "formatted-datetime" to checking
        MOVE "1900-01-01T00:00:00-01:00" TO SHOULD-BE
        MOVE FUNCTION formatted-datetime("YYYY-MM-DDThh:mm:ss+hh:mm" 0109208 0 -60 ) TO but-is
        perform checkit.

        MOVE "1900-01-01T00:00:00+00:00" TO SHOULD-BE
        MOVE FUNCTION formatted-datetime("YYYY-MM-DDThh:mm:ss+hh:mm" 0109208 0 -0 )  TO but-is
        perform checkit.

        MOVE "1900-01-01T00:00:00+01:00" TO SHOULD-BE
        MOVE FUNCTION formatted-datetime("YYYY-MM-DDThh:mm:ss+hh:mm" 0109208 0 +60 ) TO but-is
        perform checkit.

        move "SECONDS-FROM-FORMATTED-TIME" to checking
        MOVE "00043200.000000" TO SHOULD-BE
        MOVE SPACE TO but-is
        MOVE FUNCTION SECONDS-FROM-FORMATTED-TIME("hh:mm:ss" "12:00:00") TO but-is-n
        perform checkit.

        stop run.

        checkit.
      *>      display "checkit " """" should-be """" space """" but-is """"
            if FUNCTION TRIM(should-be) IS NUMERIC AND FUNCTION TRIM(but-is) IS NUMERIC
                if FUNCTION NUMVAL(should-be)
                        not equal to FUNCTION NUMVAL(but-is)
                    and should-be not equal to but-is
                    then
                    display function trim (checking) ":"
                            " should be " """" function trim (should-be) """"
                            " but is "    """" function trim (but-is) """"
                    move 1 to return-code
                    end-if
            else
                if should-be not equal to but-is
                    and should-be not equal to but-is
                    then
                    display function trim (checking) ":"
                            " should be " """" function trim (should-be) """"
                            " but is "    """" function trim (but-is) """"
                    move 1 to return-code
                end-if
        .
        blot-zulu.
            move "hh:mm" TO but-is(12:5)
            move "hh:mm" TO should-be(12:5)
            if but-is(21:1) not equal to space
                move "+hh:mm" TO but-is(20:6)
                move "+hh:mm" TO should-be(20:6)
                end-if
        .
        end program test.

