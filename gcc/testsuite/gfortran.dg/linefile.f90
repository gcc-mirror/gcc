! { dg-do compile }
! { dg-options "-Wall" }

! This will verify that the # <line> <file> directive later does not
! mess up the diagnostic on this line
SUBROUTINE s(dummy) ! { dg-warning "Unused" }
  INTEGER, INTENT(in) :: dummy
END SUBROUTINE

# 12345 "foo-f"
SUBROUTINE s2(dummy)
  INTEGER, INTENT(in) :: dummy
END SUBROUTINE
! We want to check that the # directive changes the filename in the
! diagnostic.  Nothing else really matters here.  dg-regexp allows us
! to see the entire diagnostic.  We just have to make sure to consume
! the entire message.
! { dg-regexp "foo-f\[^\n]*" }
