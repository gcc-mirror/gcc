! Related to PR 15326.  Try calling string functions whose lengths involve
! some sort of array calculation.
! { dg-do run }
pure elemental function double (x)
  integer, intent (in) :: x
  integer :: double
  double = x * 2
end function double

program main
  implicit none

  interface
    pure elemental function double (x)
      integer, intent (in) :: x
      integer :: double
    end function double
  end interface

  integer, dimension (100:104), target :: a
  integer, dimension (:), pointer :: ap
  integer :: i, lower

  a = (/ (i + 5, i = 0, 4) /)
  ap => a
  lower = 11

  call test (f1 (a), 35)
  call test (f1 (ap), 35)
  call test (f1 ((/ 5, 10, 50 /)), 65)
  call test (f1 (a (101:103)), 21)

  call test (f2 (a), 115)
  call test (f2 (ap), 115)
  call test (f2 ((/ 5, 10, 50 /)), 119)
  call test (f2 (a (101:103)), 116)

  call test (f3 (a), 60)
  call test (f3 (ap), 60)
  call test (f3 ((/ 5, 10, 50 /)), 120)
  call test (f3 (a (101:103)), 30)

  call test (f4 (a, 13, 1), 21)
  call test (f4 (ap, 13, 2), 14)
  call test (f4 ((/ 5, 10, 50 /), 12, 1), 60)
  call test (f4 (a (101:103), 12, 1), 15)
contains
  function f1 (array)
    integer, dimension (10:) :: array
    character (len = sum (array)) :: f1
    f1 = ''
  end function f1

  function f2 (array)
    integer, dimension (10:) :: array
    character (len = array (11) + a (104) + 100) :: f2
    f2 = ''
  end function f2

  function f3 (array)
    integer, dimension (:) :: array
    character (len = sum (double (array (2:)))) :: f3
    f3 = ''
  end function f3

  function f4 (array, upper, stride)
    integer, dimension (10:) :: array
    integer :: upper, stride
    character (len = sum (array (lower:upper:stride))) :: f4
    f4 = ''
  end function f4

  subroutine test (string, length)
    character (len = *) :: string
    integer, intent (in) :: length
    if (len (string) .ne. length) STOP 1
  end subroutine test
end program main
