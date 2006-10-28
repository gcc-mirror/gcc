! { dg-do run }
! { dg-shouldfail "Real BOZ not allowed" }
! { dg-options "-fall-intrinsics -std=f2003" }
! Test for invalid (F95/F2003) writing of real with octal edit descriptor
! PR fortran/29625
program real_boz
  implicit none
  real(4)           :: r
  character(len=100) :: str

  r = 325.56
  write(str,'(o0)') r
end program real_boz
! { dg-output "At line 12 .*" }
! { dg-output "Expected INTEGER .* in formatted transfer, got REAL" }
