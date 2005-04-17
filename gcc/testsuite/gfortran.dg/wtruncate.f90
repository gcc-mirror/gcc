! { dg-do compile }
! { dg-options "-Wall" }

! This long comment line should not trigger a line-truncation warning with -Wall even for free-form 132 character line limit (blah blah)

      PROGRAM foo
      WRITE (*,*) "Test"           ! Neither this comment which exceeds the 132 character limit with some random words, too (blah blah)
      WRITE (*,*) "This exactly 132 character long soruce line not, too.  How can people fill 132 characters without sensless stuff"
      END

