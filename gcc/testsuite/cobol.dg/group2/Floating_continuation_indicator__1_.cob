       *> { dg-do run }
       *> { dg-options "-ffixed-form" }
       *> { dg-output-file "group2/Floating_continuation_indicator__1_.out" }
       IDENTIFICATION DIVISION.
      * testing floating continuation literals ("'-" and '"-')
       PROGRAM-ID. FF2.
       PROCEDURE DIVISION.
           DISPLAY "hello "-
             "world.".
           DISPLAY 'hello '-
             'world.'.
           DISPLAY "hello "-
      * non-interrupting comment
             "world.".
           DISPLAY 'hello '-
             *> non-interrupting comment

             'world.'.
           EXIT PROGRAM.


