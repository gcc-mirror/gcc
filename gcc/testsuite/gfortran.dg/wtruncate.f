! { dg-do compile }
! { dg-options "-Wall" }

! This long comment line should not trigger a line-truncation warning with -Wall

      PROGRAM foo
      WRITE (*,*) "Test"           ! Neither this comment which exceeds the 72 character limit, too
      WRITE (*,*) "This exactly 72 character long source line not, too."
      END

