! { dg-do compile }
! { dg-options "-fsecond-underscore" }
! PR fortran/95106

module m2345678901234567890123456789012345678901234567890123456789_123
  implicit none
  real :: a(4), u(3,2)
  real :: b(4), v(4,2)
  equivalence (a(1),u(1,1)), (b(1),v(1,1))
end
! { dg-final { scan-assembler {m2345678901234567890123456789012345678901234567890123456789_123.eq.0__} } }
! { dg-final { scan-assembler {m2345678901234567890123456789012345678901234567890123456789_123.eq.1__} } }
