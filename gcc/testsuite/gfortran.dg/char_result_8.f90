! Related to PR 15326.  Compare functions that return string pointers with
! functions that return strings.
! { dg-do run }
program main
  implicit none

  character (len = 30), target :: string

  call test (f1 (), 30)
  call test (f2 (50), 50)
  call test (f3 (), 30)
  call test (f4 (70), 70)

  call indirect (100)
contains
  function f1
    character (len = 30) :: f1
    f1 = ''
  end function f1

  function f2 (i)
    integer :: i
    character (len = i) :: f2
    f2 = ''
  end function f2

  function f3
    character (len = 30), pointer :: f3
    f3 => string
  end function f3

  function f4 (i)
    integer :: i
    character (len = i), pointer :: f4
    f4 => string
  end function f4

  subroutine indirect (i)
    integer :: i
    call test (f1 (), 30)
    call test (f2 (i), i)
    call test (f3 (), 30)
    call test (f4 (i), i)
  end subroutine indirect

  subroutine test (string, length)
    character (len = *) :: string
    integer, intent (in) :: length
    if (len (string) .ne. length) call abort
  end subroutine test
end program main
