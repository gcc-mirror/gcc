! { dg-do compile }
! { dg-options "-funsigned" }
module mytype
  integer, parameter :: uk = selected_unsigned_kind(12)
end module mytype

module foo
  use mytype
  implicit none
  unsigned(uk), parameter :: seed0 = 1u_uk
  unsigned(uk), protected :: x_ = seed0
end module foo
