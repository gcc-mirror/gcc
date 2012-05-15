! { dg-do compile }
!
! PR 36704: Procedure pointer as function result
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module proc_ptr_15

  interface
    function e(x)
      real :: x
      procedure(), pointer :: e
    end function e
  end interface

  interface
    function f(x)
      real :: x
      external :: f
      pointer :: f
    end function
  end interface

  interface
    function g(x)
      real :: x
      pointer :: g
      external :: g
    end function
  end interface

contains

  subroutine point_fun()
    call set_fun(aux)
  end subroutine

  subroutine set_fun(y)
    external :: y
  end subroutine

  function aux()
    external aux
    pointer aux
    intrinsic sin
    aux => sin
  end function

  function foo(x)
    real :: x
    interface
      subroutine foo(i)  ! { dg-error "attribute conflicts with" }
        integer :: i
      end subroutine
    end interface
    !pointer :: foo
  end function

end
