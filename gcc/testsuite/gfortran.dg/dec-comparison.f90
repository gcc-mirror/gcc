! { dg-do compile }
! { dg-options "-fdec" }
!
! Test case contributed by Mark Eggleston  <mark.eggleston@codethink.com>
!
! Hollerith constants and character literals are allowed in comparisons,
! check that character variables can not be compared with numeric variables.

program convert
  character(4) :: a = 4hJMAC
  integer(4) :: b = "JMAC"
  real(4) :: c = "JMAC"
  complex(4) :: d = "JMACJMAC"
  ! integers
  if (a.ne.b) stop 1 ! { dg-error "Operands of comparison" }
  if (b.eq.a) stop 2 ! { dg-error "Operands of comparison" }
  if (a.ge.b) stop 3 ! { dg-error "Operands of comparison" }
  if (b.ge.a) stop 4 ! { dg-error "Operands of comparison" }
  if (a.gt.b) stop 5 ! { dg-error "Operands of comparison" }
  if (b.gt.a) stop 6 ! { dg-error "Operands of comparison" }
  if (a.le.b) stop 3 ! { dg-error "Operands of comparison" }
  if (b.le.a) stop 4 ! { dg-error "Operands of comparison" }
  if (a.lt.b) stop 5 ! { dg-error "Operands of comparison" }
  if (b.lt.a) stop 6 ! { dg-error "Operands of comparison" }
  ! reals
  if (a.ne.c) stop 7 ! { dg-error "Operands of comparison" }
  if (c.eq.a) stop 8 ! { dg-error "Operands of comparison" }
  if (a.ge.c) stop 9 ! { dg-error "Operands of comparison" }
  if (c.ge.a) stop 10 ! { dg-error "Operands of comparison" }
  if (a.gt.c) stop 11 ! { dg-error "Operands of comparison" }
  if (c.gt.a) stop 12 ! { dg-error "Operands of comparison" }
  if (a.le.c) stop 13 ! { dg-error "Operands of comparison" }
  if (c.le.a) stop 14 ! { dg-error "Operands of comparison" }
  if (a.lt.c) stop 15 ! { dg-error "Operands of comparison" }
  if (c.lt.a) stop 16 ! { dg-error "Operands of comparison" }
  ! complexes
  a = "JMACJMAC"
  if (a.ne.d) stop 17 ! { dg-error "Operands of comparison" }
  if (d.eq.a) stop 18 ! { dg-error "Operands of comparison" }
end program
