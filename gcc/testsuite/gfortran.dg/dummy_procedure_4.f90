! { dg-do compile }
!
! PR 46067: [F03] invalid procedure pointer assignment not detected
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

  type test_type
    integer :: id = 1
  end type

contains

  real function fun1 (t,x)
    real, intent(in) :: x
    type(test_type) :: t
    print *," id = ", t%id
    fun1 = cos(x)
  end function

end module


  use m
  implicit none

  call test (fun1)  ! { dg-error "Interface mismatch in dummy procedure" }

contains

  subroutine test(proc)
    interface
      real function proc(t,x)
        import :: test_type
        real, intent(in) :: x
        class(test_type) :: t
      end function
    end interface
    type(test_type) :: funs
    real :: r
    r = proc(funs,0.)
    print *, " proc(0) ",r
  end subroutine

end

! { dg-final { cleanup-modules "m" } }
