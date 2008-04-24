! { dg-do compile }
! { dg-require-effective-target fortran_large_real }
!
subroutine test_large
  integer, parameter :: wp = selected_real_kind (precision (0.0_8) + 1)
  complex(wp), parameter :: i = (0._wp, 1._wp)
  complex(wp) :: c(12)
  integer :: m, N

  N = 12
  c = (/(exp(i*m),m=1,N)/)
  print *, c(1)
end
