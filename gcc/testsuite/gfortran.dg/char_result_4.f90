! Like char_result_3.f90, but the array arguments are pointers.
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
  lower = lbound(a,dim=1)

  call test (f1 (ap), 35)
  call test (f2 (ap), 115)
  call test (f3 (ap), 60)
  call test (f4 (ap, 104, 2), 21)
contains
  function f1 (array)
    integer, dimension (:), pointer :: array
    character (len = sum (array)) :: f1
    f1 = ''
  end function f1

  function f2 (array)
    integer, dimension (:), pointer :: array
    character (len = array (101) + a (104) + 100) :: f2
    f2 = ''
  end function f2

  function f3 (array)
    integer, dimension (:), pointer :: array
    character (len = sum (double (array (101:)))) :: f3
    f3 = ''
  end function f3

  function f4 (array, upper, stride)
    integer, dimension (:), pointer :: array
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
