! { dg-do compile }
! { dg-options "-Wall" }
! PR fortran/49693 - this used to cause a spurious warning for the
! variable in the common block.
! Test case by Stephan Kramer.
module foo
  implicit none
  integer:: a, b
  common a
end module foo
