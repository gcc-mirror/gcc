! { dg-do compile }
! PR fortran/36192.f90
!
program three_body
  real, parameter :: n = 2, d = 2
  real, dimension(n,d) :: x  ! { dg-error "Expecting a scalar INTEGER" }
  x(1,:) = (/ 1.0, 0.0 /)    ! { dg-error "Unclassifiable" }
end program three_body
