! Test for the ISNAN intrinsic on constants
!
! { dg-do run }
! { dg-options "-fno-range-check" }
! { dg-options "-fno-range-check -pedantic-errors -mieee" { target alpha*-*-* sh*-*-* } }
! { dg-skip-if "NaN not supported" { spu-*-* } { "*" } { "" } }
!
  implicit none
  character(len=1) :: s
  write(s,'(L1)') isnan(0.)
  if (s /= 'F') call abort

  write(s,'(L1)') isnan(exp(huge(0.)))
  if (s /= 'F') call abort

  write(s,'(L1)') isnan(0./0.)
  if (s /= 'T') call abort
end
