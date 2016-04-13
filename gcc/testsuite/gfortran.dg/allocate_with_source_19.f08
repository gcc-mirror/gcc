! { dg-do compile }
! { dg-options -std=f2008 }

! Contributed by mrestelli@gmail.com
! Check that instead of an ICE the error message is emitted.

module m
 implicit none
contains

 subroutine s()
  real, allocatable :: x(:)
  real :: y

   y = 5.0
   ! x either needs an array spec, or y needs to be an array.
   allocate( x , source=y ) ! { dg-error "Array specification or array-valued SOURCE= expression required in ALLOCATE statement" }

 end subroutine s

end module m

