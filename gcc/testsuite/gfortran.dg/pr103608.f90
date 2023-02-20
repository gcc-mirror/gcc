! { dg-do compile }
! { dg-options "-w" }
! PR fortran/103608 - ICE in do_intent
! Contributed by G.Steinmetz

program p
  implicit none
  integer :: i
  integer :: x     ! { dg-error "Alternate return specifier" }
  x(*) = 0
  do i = 1, 2
     print *, x(i) ! { dg-error "Missing alternate return specifier" }
  end do
end
