! { dg-do run }
! { dg-options "-fdefault-integer-8 -O2" }
! Tests the fix for PR34143, where the implicit type
! conversion in the derived type constructor would fail,
! when 'yy' was not allocated.  The testscase is an
! extract from alloc_comp_constructor.f90.
!
! Reported by Thomas Koenig <tkoenig@gcc.gnu.org>
!
Program test_constructor
    implicit none
    type :: thytype
        integer(4) :: a(2,2)
    end type thytype
    type :: mytype
        integer(4), allocatable :: a(:, :)
        type(thytype), allocatable :: q(:)
    end type mytype
    integer, allocatable :: yy(:,:)
    type (thytype), allocatable :: bar(:)
    call non_alloc
    call alloc
contains
    subroutine non_alloc
      type (mytype) :: x
      x = mytype(yy, bar)
      if (allocated (x%a) .or. allocated (x%q)) STOP 1
    end subroutine non_alloc
    subroutine alloc
      type (mytype) :: x
      allocate (yy(2,2))
      allocate (bar(2))
      yy = reshape ([10,20,30,40],[2,2])
      bar = thytype (reshape ([1,2,3,4],[2,2]))
      x = mytype(yy, bar)
      if (.not.allocated (x%a) .or. .not.allocated (x%q)) STOP 2
    end subroutine alloc
end program test_constructor
