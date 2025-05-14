       *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/Quick_check_of_PIC_XX_COMP-5.out" }
         identification division.
        program-id. wrapper.
        data division.
        working-storage section.
        01 memx pic x(2) comp-5.
        77 ptr pointer.
        procedure division.
        Initialize ptr.display "LENGTH OF X(2) is " length of memx
        move 12345 to memx
        display memx
        IF ptr <> NULL then display 'bad pointer'.
        goback.
        end program wrapper.

