! { dg-do compile }
! PR fortran/56007
! Based on testcase by Tobias Schlüter

  integer iw1(90), doiw1(90)
  do iw1=1,2     ! { dg-error "cannot be an array" }
  end do         ! { dg-error "Expecting END PROGRAM statement" }
  do iw1(1)=1    ! { dg-error "Unclassifiable statement" }
  do iw1=1       ! { dg-error "cannot be an array" }
  end do         ! { dg-error "Expecting END PROGRAM statement" }
END program
