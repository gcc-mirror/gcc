! { dg-do compile }
!
! Tests the fix for PR82077
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
    type parent
    end type parent
    type, extends(parent) :: child
    end type
    class(parent), allocatable :: foo(:,:)
    allocate(child::foo(1,1))
    select type(foo)
      class is (child)
        call gfortran7_ICE(foo(1,:))  ! ICEd here.
    end select
contains
    subroutine gfortran7_ICE(bar)
      class(child) bar(:)
    end subroutine
end
