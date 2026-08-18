! { dg-do compile }
! Test the a missed invalid code found while fixing PR121384
! Contributed by Mikael Morin  <mikael@gcc.gnu.org>
program test
  implicit none
  type :: t
    integer :: i,j
  end type
  type(t) :: a(5)
  class(t), allocatable :: c(:)
  a = [ t(2,3), t(5,7), t(11,13), t(17,19), t(23,29) ]
  associate (x => (a%i))
    if (rank(x) /= 1) error stop 11
    if (any(shape(x) /= [5])) error stop 12
    if (any(x /= [2,5,11,17,23])) error stop 13
    x(1) = 3 ! { dg-error "cannot be used in a variable definition context" }
  end associate
  if  (a(1)%i /= 2) print *,a(1)%i
  associate (x => (a%j))
    if (rank(x) /= 1) error stop 21
    if (any(shape(x) /= [5])) error stop 22
    if (any(x /= [3,7,13,19,29])) error stop 23
    x(1) = 4 ! { dg-error "cannot be used in a variable definition context" }
  end associate
  if  (a(1)%j /= 3) stop 24

! Check the class variants
  c = a
  associate (x => (c%i))
    if (rank(x) /= 1) error stop 31
    if (any(shape(x) /= [5])) error stop 32
    if (any(x /= [2,5,11,17,23])) error stop 33
    x(1) = 3 ! { dg-error "cannot be used in a variable definition context" }
  end associate
  if  (c(1)%i /= 2) stop 34
  associate (x => (c%j))
    if (rank(x) /= 1) error stop 41
    if (any(shape(x) /= [5])) error stop 42
    if (any(x /= [3,7,13,19,29])) error stop 43
    x(1) = 4 ! { dg-error "cannot be used in a variable definition context" }
  end associate
  if  (c(1)%j /= 3) stop 44
end program
