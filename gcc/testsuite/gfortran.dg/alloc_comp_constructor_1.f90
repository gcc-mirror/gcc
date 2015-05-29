! { dg-do run }
! { dg-options "-fdump-tree-original" }
! Test constructors of derived type with allocatable components (PR 20541).
!
! Contributed by Erik Edelmann  <eedelmann@gcc.gnu.org>
!            and Paul Thomas  <pault@gcc.gnu.org>
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

    type (thytype) :: foo = thytype(reshape ([43, 100, 54, 76], [2,2]))
    integer :: y(0:1, -1:0) = reshape ([42, 99, 55, 77], [2,2])

  BLOCK ! Add scoping unit as the vars are otherwise implicitly SAVEd

    type (mytype) :: x
    integer, allocatable :: yy(:,:)
    type (thytype), allocatable :: bar(:)
    integer :: i

    ! Check that null() works
    x = mytype(null(), null())
    if (allocated(x%a) .or. allocated(x%q)) call abort()

    ! Check that unallocated allocatables work
    x = mytype(yy, bar)
    if (allocated(x%a) .or. allocated(x%q)) call abort()

    ! Check that non-allocatables work
    x = mytype(y, [foo, foo])
    if (.not.allocated(x%a) .or. .not.allocated(x%q)) call abort()
    if (any(lbound(x%a) /= lbound(y))) call abort()
    if (any(ubound(x%a) /= ubound(y))) call abort()
    if (any(x%a /= y)) call abort()
    if (size(x%q) /= 2) call abort()
    do i = 1, 2
        if (any(x%q(i)%a /= foo%a)) call abort()
    end do

    ! Check that allocated allocatables work
    allocate(yy(size(y,1), size(y,2)))
    yy = y
    allocate(bar(2))
    bar = [foo, foo]
    x = mytype(yy, bar)
    if (.not.allocated(x%a) .or. .not.allocated(x%q)) call abort()
    if (any(x%a /= y)) call abort()
    if (size(x%q) /= 2) call abort()
    do i = 1, 2
        if (any(x%q(i)%a /= foo%a)) call abort()
    end do

    ! Functions returning arrays
    x = mytype(bluhu(), null())
    if (.not.allocated(x%a) .or. allocated(x%q)) call abort()
    if (any(x%a /= reshape ([41, 98, 54, 76], [2,2]))) call abort()

    ! Functions returning allocatable arrays
    x = mytype(blaha(), null())
    if (.not.allocated(x%a) .or. allocated(x%q)) call abort()
    if (any(x%a /= reshape ([40, 97, 53, 75], [2,2]))) call abort()

    ! Check that passing the constructor to a procedure works
    call check_mytype (mytype(y, [foo, foo]))
  END BLOCK
contains

    subroutine check_mytype(x)
        type(mytype), intent(in) :: x
        integer :: i

        if (.not.allocated(x%a) .or. .not.allocated(x%q)) call abort()
        if (any(lbound(x%a) /= lbound(y))) call abort()
        if (any(ubound(x%a) /= ubound(y))) call abort()
        if (any(x%a /= y)) call abort()
        if (size(x%q) /= 2) call abort()
        do i = 1, 2
            if (any(x%q(i)%a /= foo%a)) call abort()
        end do

    end subroutine check_mytype


    function bluhu()
        integer :: bluhu(2,2)

        bluhu = reshape ([41, 98, 54, 76], [2,2])
    end function bluhu


    function blaha()
        integer, allocatable :: blaha(:,:)

        allocate(blaha(2,2))
        blaha = reshape ([40, 97, 53, 75], [2,2])
    end function blaha

end program test_constructor
! { dg-final { scan-tree-dump-times "builtin_free" 19 "original" } }
