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

  subroutine indirect (fn)
    interface
      integer pure function fn (x)
        integer, intent (in) :: x
      end function fn
    end interface
    call test (f1 (fn, 100), 200)
  end subroutine indirect

  subroutine test (string, length)
    character (len = *) :: string
    integer, intent (in) :: length
    if (len (string) .ne. length) STOP 1
  end subroutine test
end program main
