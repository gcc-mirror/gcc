! { dg-do compile }
!
! CO_SUM/CO_MIN/CO_MAX
!
program test
  implicit none
  intrinsic co_max
  integer :: val
  call co_max(val) ! { dg-error "Coarrays disabled at .1., use '-fcoarray=' to enable" }
end program test
! { dg-prune-output "compilation terminated" }
