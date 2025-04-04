       *> { dg-do run }
       *> { dg-output-file "group2/Program-to-program_parameters_and_retvals.out" }
        IDENTIFICATION DIVISION.
        PROGRAM-ID.  prog.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  var1        pic 9               VALUE 1.
        01  var2        BINARY-CHAR         VALUE 22.
        01  var3        pic s999 COMP-3     VALUE -333.
        01  var4        pic 9999 BINARY     VALUE 4444.
        01  var5        pic 99.99           VALUE "12.34".
        01  var6        pic s999V999 COMP-5 VALUE -123.456.
        01  var7        float-short         VALUE  1.23E10.
        01  var8        float-long          VALUE  -1.23E20.
        01  var9        float-extended      VALUE  1.23E40.
        01  var64       pic  9(15) VALUE 987654321098765.
        01  var128      pic s9(30) VALUE -987654321098765432109876543210.
        01  filler.
         02 varpd       pic 9(18) comp-5 value 1250999747361.
         02 varp redefines varpd       pointer.
        01  varg.
            02 varg1 pic x(7) VALUE "That's".
            02 varg2 pic x(5) VALUE "all,"  .
            02 varg3 pic x(7) VALUE "folks!".

        01  var1r        pic 9               .
        01  var2r        BINARY-CHAR         .
        01  var3r        pic s999 COMP-3     .
        01  var4r        pic 9999 BINARY     .
        01  var5r        pic 99.99           .
        01  var6r        pic s999V999 COMP-5 .
        01  var7r        float-short         .
        01  var8r        float-long          .
        01  var9r        float-extended      .
        01  var64r       pic  9(15)          .
        01  var128r      pic s9(30)          .
        01  varpr        pointer.
        01  vargr.
            02 varg1 pic x(7).
            02 varg2 pic x(5).
            02 varg3 pic x(7).

        PROCEDURE DIVISION.
            display     var1
            call     "rvar1" USING by value var1 RETURNING var1r
            display     var1r

            display     var2
            call     "rvar2" USING by reference var2 RETURNING var2r
            display     var2r

            display     var3
            call     "rvar3" USING by content var3 RETURNING var3r
            display     var3r

            display     var4
            call     "rvar4" USING by value var4 RETURNING var4r
            display     var4r

            display     var5
            call     "rvar5" USING by reference var5 RETURNING var5r
            display     var5r

            display     var6
            call     "rvar6" USING by content var6 RETURNING var6r
            display     var6r

            display     var7
            call     "rvar7" USING by reference var7 RETURNING var7r
            display     var7r

            display     var8
            call     "rvar8" USING by value var8 RETURNING var8r
            display     var8r

            display     var9
            call     "rvar9" USING by content var9 RETURNING var9r
            display     var9r

            display     var64
            call     "rvar64" USING by value var64 RETURNING var64r
            display     var64r

            display     var128
            call     "rvar128" USING by reference var128 RETURNING var128r
            display     var128r

            display     varp
            call     "rvarp" USING by reference varp RETURNING varpr
            display     varpr

            display     """"varg""""
            call     "rvarg" USING by reference varg RETURNING vargr
            display     """"vargr""""

            GOBACK.
            END PROGRAM prog.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvar1.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var         pic 9               .
        01  varr        pic 9               .
        PROCEDURE DIVISION USING by value var RETURNING varr.
            MOVE var TO varr.
            END PROGRAM rvar1.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvar2.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var         BINARY-CHAR         .
        01  varr        BINARY-CHAR         .
        PROCEDURE DIVISION USING by reference var RETURNING varr.
            MOVE var TO varr.
            END PROGRAM rvar2.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvar3.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var         pic s999 COMP-3     .
        01  varr        pic s999 COMP-3     .
        PROCEDURE DIVISION USING by reference var RETURNING varr.
            MOVE var TO varr.
            END PROGRAM rvar3.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvar4.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var         pic 9999 BINARY     .
        01  varr        pic 9999 BINARY     .
        PROCEDURE DIVISION USING by value var RETURNING varr.
            MOVE var TO varr.
            END PROGRAM rvar4.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvar5.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var         pic 99.99           .
        01  varr        pic 99.99           .
        PROCEDURE DIVISION USING by reference var RETURNING varr.
            MOVE var TO varr.
            END PROGRAM rvar5.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvar6.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var         pic s999V999 COMP-5 .
        01  varr        pic s999V999 COMP-5 .
        PROCEDURE DIVISION USING reference var RETURNING varr.
            MOVE var TO varr.
            END PROGRAM rvar6.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvar7.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var         float-short          .
        01  varr        float-short          .
        PROCEDURE DIVISION USING by reference VAR RETURNING varr.
            MOVE var TO varr.
            GOBACK.
            END PROGRAM rvar7.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvar8.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var         float-long          .
        01  varr        float-long          .
        PROCEDURE DIVISION USING by value var RETURNING varr.
            MOVE var TO varr.
            END PROGRAM rvar8.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvar9.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var         float-extended      .
        01  varr        float-extended      .
        PROCEDURE DIVISION USING by reference var RETURNING varr.
            MOVE var TO varr.
            END PROGRAM rvar9.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvar64.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var        pic  9(15)          .
        01  varr       pic  9(15)          .
        PROCEDURE DIVISION USING by value var RETURNING varr.
            MOVE var TO varr.
            END PROGRAM rvar64.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvar128.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var  pic s9(30) .
        01  varr pic s9(30) .
        PROCEDURE DIVISION USING by reference var RETURNING varr.
            MOVE var TO varr.
            END PROGRAM rvar128.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvarp.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var  pointer .
        01  varr pointer .
        PROCEDURE DIVISION USING by reference var RETURNING varr.
            SET varr TO var.
            END PROGRAM rvarp.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. rvarg.
        DATA DIVISION.
        LINKAGE SECTION.
        01  var.
            02 varg1 pic x(7).
            02 varg2 pic x(5).
            02 varg3 pic x(7).
        01  varr.
            02 varg1 pic x(7).
            02 varg2 pic x(5).
            02 varg3 pic x(7).
        PROCEDURE DIVISION USING by reference var RETURNING varr.
            MOVE var TO varr.
            END PROGRAM rvarg.

