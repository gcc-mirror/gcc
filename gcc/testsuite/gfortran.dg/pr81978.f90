! { dg-do run }
! PR fortran/81978 - do not copy back for parameter actual arguments

module test_mod
  implicit none

  type pp_struct
     character(10) :: name
     real          :: value
  end type pp_struct

  type(pp_struct), parameter :: pp(4) = [ &
       pp_struct('one',   1.), &
       pp_struct('two',   2.), &
       pp_struct('three', 3.), &
       pp_struct('four',  4.)  ]

contains

  subroutine match_word (names)
    character(*)        :: names(:)
  end subroutine match_word

  subroutine sub0 (a)
    real                :: a(:)
  end

  subroutine sub1 (a, n)
    integer, intent(in) :: n
    real                :: a(n)
  end

  subroutine subx (a)
    real                :: a(..)
  end
end module

program test
  use test_mod
  implicit none
  integer :: i, n
  integer, parameter :: m = 8
  real,    parameter :: x(m) = [(i,i=1,m)]

  n = size (x)
  call sub0 (x)
  call sub1 (x, n)
  call sub2 (x, n)
  call subx (x)

  i = 1
  call sub0 (x(1::i))
  call sub1 (x(1::i), n)
  call sub2 (x(1::i), n)
  call subx (x(1::i))

  n = size (x(1::2))
  call sub0 (x(1::2))
  call sub1 (x(1::2), n)
  call sub2 (x(1::2), n)
  call subx (x(1::2))

  i = 2
  call sub0 (x(1::i))
  call sub1 (x(1::i), n)
  call sub2 (x(1::i), n)
  call subx (x(1::i))

  call match_word (pp%name)
  call sub0       (pp%value)
  call subx       (pp%value)
  call match_word (pp(1::2)%name)
  call sub0       (pp(1::2)%value)
  call subx       (pp(1::2)%value)
  i = 1
  call match_word (pp(1::i)%name)
  call sub0       (pp(1::i)%value)
  call subx       (pp(1::i)%value)
  i = 2
  call match_word (pp(1::i)%name)
  call sub0       (pp(1::i)%value)
  call subx       (pp(1::i)%value)

  call foo  (pp%name,        size(pp%name))
  call foo  (pp(1::2)%name,  size(pp(1::2)%name))
  call sub1 (pp(1::2)%value, size(pp(1::2)%value))
  call sub2 (pp(1::2)%value, size(pp(1::2)%value))
  i = 1
  call foo  (pp(1::i)%name,  size(pp(1::i)%name))
  call sub1 (pp(1::i)%value, size(pp(1::i)%value))
  call sub2 (pp(1::i)%value, size(pp(1::i)%value))
  i = 2
  call foo  (pp(1::i)%name,  size(pp(1::i)%name))
  call sub1 (pp(1::i)%value, size(pp(1::i)%value))
  call sub2 (pp(1::i)%value, size(pp(1::i)%value))
end program

subroutine sub2 (a, n)
  integer, intent(in) :: n
  real                :: a(n)
end

subroutine foo (s, n)
  integer, intent(in) :: n
  character(*)        :: s(*)
! print *, len(s), n, s(n)
end
