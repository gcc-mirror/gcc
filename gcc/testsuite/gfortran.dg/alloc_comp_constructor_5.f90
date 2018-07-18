! { dg-do run }
! { dg-options "-fdefault-integer-8" }
! Tests the fix for PR34143, in which the implicit conversion of yy, with
! fdefault-integer-8, would cause a segfault at runtime.
!
! Contributed by Thomas Koenig <tkoenig@gcc.gnu.org>
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
    type (mytype) :: x, y
    x = mytype(yy, bar)
    if (allocated (x%a) .or. allocated (x%q)) STOP 1
    allocate (yy(2,2))
    allocate (bar(2))
    yy = reshape ([10,20,30,40],[2,2])
    bar = thytype (reshape ([1,2,3,4],[2,2]))
    ! Check that unallocated allocatables work
    y = mytype(yy, bar)
    if (.not.allocated (y%a) .or. .not.allocated (y%q)) STOP 2
end program test_constructor
