      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect iso" }

        identification division.
        program-id. prog.
        procedure division.
           READY TRACE.
           para-1.
             DISPLAY "OK" NO ADVANCING.
           RESET TRACE.
           para-2.
             DISPLAY "OK" NO ADVANCING.
           GOBACK.

