! { dg-do compile }
!
! Argument checking; dim and kind have to be scalar
!
! PR fortran/33297
!
  integer array(5), i1, i2
  print *, size(array,(/i1,i2/))  ! { dg-error "must be a scalar" }
  print *, size(array,i1,(/i1,i2/))  ! { dg-error "must be a scalar" }
  end
