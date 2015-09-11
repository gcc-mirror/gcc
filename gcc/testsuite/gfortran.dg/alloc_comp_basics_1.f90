! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Check some basic functionality of allocatable components, including that they
! are nullified when created and automatically deallocated when
! 1. A variable goes out of scope
! 2. INTENT(OUT) dummies
! 3. Function results
!
!
! Contributed by Erik Edelmann  <eedelmann@gcc.gnu.org>
!            and Paul Thomas  <pault@gcc.gnu.org>
!
module alloc_m

    implicit none

    type :: alloc1
        real, allocatable :: x(:)
    end type alloc1

end module alloc_m


program alloc

    use alloc_m

    implicit none

    type :: alloc2
        type(alloc1), allocatable :: a1(:)
        integer, allocatable :: a2(:)
    end type alloc2

    integer :: i

  BLOCK  ! To ensure that the allocatables are freed at the end of the scope
    type(alloc2) :: b
    type(alloc2), allocatable :: c(:)

    if (allocated(b%a2) .OR. allocated(b%a1)) then
        write (0, *) 'main - 1'
        call abort()
    end if

    ! 3 calls to _gfortran_deallocate (INTENT(OUT) dummy)
    call allocate_alloc2(b)
    call check_alloc2(b)

    do i = 1, size(b%a1)
        ! 1 call to _gfortran_deallocate
        deallocate(b%a1(i)%x)
    end do

    ! 3 calls to _gfortran_deallocate (INTENT(OUT) dummy)
    call allocate_alloc2(b)

    call check_alloc2(return_alloc2())
    ! 3 calls to _gfortran_deallocate (function result)

    allocate(c(1))
    ! 3 calls to _gfortran_deallocate (INTENT(OUT) dummy)
    call allocate_alloc2(c(1))
    ! 4 calls to _gfortran_deallocate
    deallocate(c)

    ! 7 calls to _gfortran_deallocate (b (3) and c(4) goes aout of scope)
  END BLOCK
contains

    subroutine allocate_alloc2(b)
        type(alloc2), intent(out) :: b
        integer :: i

        if (allocated(b%a2) .OR. allocated(b%a1)) then
            write (0, *) 'allocate_alloc2 - 1'
            call abort()
        end if

        allocate (b%a2(3))
        b%a2 = [ 1, 2, 3 ]

        allocate (b%a1(3))

        do i = 1, 3
            if (allocated(b%a1(i)%x)) then
                write (0, *) 'allocate_alloc2 - 2', i
                call abort()
            end if
            allocate (b%a1(i)%x(3))
            b%a1(i)%x = i + [ 1.0, 2.0, 3.0 ]
        end do

    end subroutine allocate_alloc2


    type(alloc2) function return_alloc2() result(b)
        if (allocated(b%a2) .OR. allocated(b%a1)) then
            write (0, *) 'return_alloc2 - 1'
            call abort()
        end if

        allocate (b%a2(3))
        b%a2 = [ 1, 2, 3 ]

        allocate (b%a1(3))

        do i = 1, 3
            if (allocated(b%a1(i)%x)) then
                write (0, *) 'return_alloc2 - 2', i
                call abort()
            end if
            allocate (b%a1(i)%x(3))
            b%a1(i)%x = i + [ 1.0, 2.0, 3.0 ]
        end do
    end function return_alloc2


    subroutine check_alloc2(b)
        type(alloc2), intent(in) :: b

        if (.NOT.(allocated(b%a2) .AND. allocated(b%a1))) then
            write (0, *) 'check_alloc2 - 1'
            call abort()
        end if
        if (any(b%a2 /= [ 1, 2, 3 ])) then
            write (0, *) 'check_alloc2 - 2'
            call abort()
        end if
        do i = 1, 3
            if (.NOT.allocated(b%a1(i)%x)) then
                write (0, *) 'check_alloc2 - 3', i
                call abort()
            end if
            if (any(b%a1(i)%x /= i + [ 1.0, 2.0, 3.0 ])) then
                write (0, *) 'check_alloc2 - 4', i
                call abort()
            end if
        end do
    end subroutine check_alloc2

end program alloc
! { dg-final { scan-tree-dump-times "builtin_free" 18 "original" } }
