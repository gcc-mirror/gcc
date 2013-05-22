! { dg-do compile }
!
! PR fortran/
!
! Contributed by Vladimír Fuka
!
function avg(a)
  integer :: avg
  integer,intent(in) :: a(..)
  
  avg = sum(a)/size(a) ! { dg-error "Assumed-rank argument at .1. is only permitted as actual argument to intrinsic inquiry functions" }
end function
