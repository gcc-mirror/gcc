! { dg-do compile }
!
! Test fix for PR103748.
!
! Contributed by Bastiaan Braams  <b.j.braams@cwi.nl>
!
program test
  implicit none
  type f_type
     integer, allocatable :: x(:)
  end type f_type
  type (f_type(n=9)) :: f ! { dg-error "is not parameterized" }
  stop
end program test
