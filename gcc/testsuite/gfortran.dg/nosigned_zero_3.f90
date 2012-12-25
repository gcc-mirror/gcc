! { dg-do run }
! { dg-options "-fno-sign-zero" }
!
! PR fortran/55539
!
program nosigned_zero_3
  implicit none
  character(len=20) :: s
  real(4) :: x = -1.2e-3
  real(8) :: y = -1.2e-3
  write(s,'(7f10.3)') x
  if (trim(adjustl(s)) /= "-0.001") call abort
  write(s, '(7f10.3)') y
  if (trim(adjustl(s)) /= "-0.001") call abort
end program nosigned_zero_3
