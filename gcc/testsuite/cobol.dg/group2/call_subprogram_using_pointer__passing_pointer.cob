       *> { dg-do run }
       *> { dg-output-file "group2/call_subprogram_using_pointer__passing_pointer.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 varp program-pointer.
       PROCEDURE DIVISION.
          SET varp TO ENTRY "ref".
          CALL "sub" USING BY VALUE varp.
       end program prog.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. sub.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 param pic x(12) value "hi".
       LINKAGE SECTION.
       77 varp program-pointer.
       PROCEDURE DIVISION USING BY VALUE varp.
          DISPLAY "About to call 'ref hi' directly"
          CALL "ref" USING param.
          DISPLAY "About to call 'ref hi' indirectly"
          CALL varp USING param.
       end program sub.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ref.
       DATA DIVISION.
       LINKAGE SECTION.
       77 greeting pic x(12).
       PROCEDURE DIVISION using greeting.
          DISPLAY """" greeting """".
       end program ref.

