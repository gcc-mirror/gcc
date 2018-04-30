! Test for the ISNAN intrinsic on constants
!
! { dg-do run }
! { dg-options "-fno-range-check" }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
!
  implicit none
  character(len=1) :: s
  write(s,'(L1)') isnan(0.)
  if (s /= 'F') STOP 1

  write(s,'(L1)') isnan(exp(huge(0.)))
  if (s /= 'F') STOP 2

  write(s,'(L1)') isnan(0./0.)
  if (s /= 'T') STOP 3
end
