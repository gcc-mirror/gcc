! { dg-do compile }
!
! PR fortran/126892
! The translation of the IS_CONTIGUOUS call used to cause an internal error
! because the variable used as descriptor for A was one pointer dereference
! away from the real descriptor.

subroutine s(a)
  integer, pointer, intent(in) :: a(..)
  print *, is_contiguous(a)
end subroutine
