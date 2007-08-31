! { dg-do compile }
!
! Cf. PR fortran/33232
program test
  implicit none
  integer :: a
  READ *, a
  READ '(i3)', a
end program test
