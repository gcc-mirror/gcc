! { dg-do compile }
! Check a few constraints for ALLOCATABLE dummy arguments.
program alloc_dummy

    implicit none
    integer :: a(5)

    call init(a) ! { dg-error "must be ALLOCATABLE" }

contains

    subroutine init(x)
        integer, allocatable, intent(out) :: x(:)
    end subroutine init

    subroutine init2(x)
        integer, allocatable, intent(in) :: x(:)

        allocate(x(3)) ! { dg-error "Cannot allocate" }
    end subroutine init2

    subroutine kill(x)
        integer, allocatable, intent(in) :: x(:)
        
        deallocate(x) ! { dg-error "Cannot deallocate" }
    end subroutine kill

end program alloc_dummy
