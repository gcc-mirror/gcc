       *> { dg-do run }
       *> { dg-output-file "group2/FLOAT-SHORT_with_SIZE_ERROR.out" }

       identification division.
       program-id. prog.

       data division.
       working-storage section.
      *------------------------
       77 counter             pic s9(4) binary value zero.
      * FLOAT-SHORT (if binary-comp-1 is not active)
       77 floatValue          COMP-1  value 2.
       77 lastFloatValue      COMP-1.

      ******************************************************************
       procedure division.
       main section.
           perform varying counter from 1 by 1 until
                           counter > 130
      *>      display 'counter: ' counter ', value: ' floatValue
              compute floatValue = floatValue * 2
                   ON SIZE ERROR
                      display 'SIZE ERROR, last value = ' floatValue
                      exit perform
               not ON SIZE ERROR
                      if floatValue > lastFloatValue
                         move floatValue to lastFloatValue
                      else
                         display 'math ERROR, last value > current: '
                                 lastFloatValue ' > ' floatValue
                         exit perform
                      end-if
              end-compute
           end-perform
           if counter not = 127
              display 'counter is ' counter
           end-if

           goback.

