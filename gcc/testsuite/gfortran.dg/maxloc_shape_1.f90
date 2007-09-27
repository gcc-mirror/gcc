! { dg-do compile }
! Tests the implementation of compile-time shape testing, required to fix
! PR19015.  The functionality of maxloc and friends is tested by existing
! testcases.
!
! Contributed by Thomas Koeing  <Thomas.Koenig@online.de>
!
  integer, dimension(0:1,0:1) :: n
  integer, dimension(1) :: i
  n = reshape((/1, 2, 3, 4/), shape(n))
  i = maxloc(n) ! { dg-error "Different shape for array assignment" }
  i = maxloc(n,dim=1) ! { dg-error "Different shape for array assignment" }
!  print *,i
end program
