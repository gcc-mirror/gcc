! { dg-do compile }
!
! Tests the fix for PR80260
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
    type foo
    end type foo
    type, extends(foo) :: bar
    end type
contains
    subroutine f(x)
      class(foo) x(:,:)
      select type(x)
        class is (bar)
          call g(x(1,:))   ! ICEd here.
      end select
    end subroutine
    subroutine g(y)
      class(bar) y(:)
    end subroutine
end
