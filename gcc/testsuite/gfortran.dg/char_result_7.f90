! Related to PR 15326.  Try calling string functions whose lengths depend
! on a dummy procedure.
! { dg-do run }
integer pure function double (x)
  integer, intent (in) :: x
  double = x * 2
end function double

program main
  implicit none

  interface
    integer pure function double (x)
      integer, intent (in) :: x
    end function double
  end interface

  call test (f1 (double, 100), 200)
  call test (f2 (double, 70), 140)

  call indirect (double)
contains
  function f1 (fn, i)
    integer :: i
    interface
      integer pure function fn (x)
        integer, intent (in) :: x
      end function fn
    end interface
    character (len = fn (i)) :: f1
    f1 = ''
  end function f1

  function f2 (fn, i)
    integer :: i, fn
    character (len = fn (i)) :: f2
    f2 = ''
  end function f2

  subroutine indirect (fn)
    interface
      integer pure function fn (x)
        integer, intent (in) :: x
      end function fn
    end interface
    call test (f1 (fn, 100), 200)
    call test (f2 (fn, 70), 140)
  end subroutine indirect

  subroutine test (string, length)
    character (len = *) :: string
    integer, intent (in) :: length
    if (len (string) .ne. length) call abort
  end subroutine test
end program main
