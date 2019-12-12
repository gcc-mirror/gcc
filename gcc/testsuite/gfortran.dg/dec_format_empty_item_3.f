! { dg-do compile }
! { dg-options "-fdec -fno-dec-blank-format-item" }
!
! Test blank/empty format items in format string
!
! Test case contributed by Jim MacArthur <jim.macarthur@codethink.co.uk>
! Modified by Mark Eggleston <mark.eggleston@codethink.com>
!
        PROGRAM blank_format_items
          INTEGER A/0/

          OPEN(1, status="scratch")
          WRITE(1, 10) 100 ! { dg-error "FORMAT label 10 at" }
          REWIND(1)
          READ(1, 10) A ! { dg-error "FORMAT label 10 at" }
          IF (a.NE.100) STOP 1
          PRINT 10, A ! { dg-error "FORMAT label 10 at" }
 10       FORMAT( I5,) ! { dg-error "Missing item" }
        END
