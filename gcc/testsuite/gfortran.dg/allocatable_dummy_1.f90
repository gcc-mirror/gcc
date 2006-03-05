! { dg-do run }
! Test procedures with allocatable dummy arguments
program alloc_dummy

    implicit none
    integer, allocatable :: a(:)

    call init(a)
    if (.NOT.allocated(a)) call abort()
    if (.NOT.all(a == [ 1, 2, 3 ])) call abort()

    call kill(a)
    if (allocated(a)) call abort()


contains

    subroutine init(x)
        integer, allocatable, intent(out) :: x(:)

        allocate(x(3))
        x = [ 1, 2, 3 ]
    end subroutine init

    
    subroutine kill(x)
        integer, allocatable, intent(out) :: x(:)

        deallocate(x)
    end subroutine kill

end program alloc_dummy
