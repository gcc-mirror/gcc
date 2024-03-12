! { dg-do compile }
! { dg-options "-w" }
!
! PR fortran/95374
! PR fortran/104352 - Various ICEs for bounds violation with zero-sized arrays
!
! Contributed by G. Steinmetz

program p
  implicit none
  integer :: i
  integer, parameter :: a(0)    = 0
  integer, parameter :: b(0:-5) = 0
  integer, parameter :: c(*) = [(a(i:i), i=0,0)] ! { dg-error "out of bounds" }
  integer, parameter :: d(*) = [(b(i:i), i=1,1)] ! { dg-error "out of bounds" }
  integer, parameter :: e(1) = [(a(i)  , i=1,1)] ! { dg-error "out of bounds" }
  integer, parameter :: f(1) = [(a(i:i), i=1,1)] ! { dg-error "out of bounds" }
  integer            :: g(1) = [(a(i:i), i=0,0)] ! { dg-error "out of bounds" }
  integer            :: h(1) = [(a(i:i), i=1,1)] ! { dg-error "out of bounds" }
  print *, [(a(i:i), i=0,0)] ! { dg-error "out of bounds" }
  print *, [(a(i:i), i=1,1)] ! { dg-error "out of bounds" }
  print *, any (a(1:1) == 1) ! { dg-error "out of bounds" }
  print *, all (a(0:0) == 1) ! { dg-error "out of bounds" }
  print *, sum (a(1:1))      ! { dg-error "out of bounds" }
  print *, iall (a(0:0))     ! { dg-error "out of bounds" }
  print *, minloc (a(0:0),1) ! { dg-error "out of bounds" }
  print *, dot_product(a(1:1),a(1:1)) ! { dg-error "out of bounds" }
end
