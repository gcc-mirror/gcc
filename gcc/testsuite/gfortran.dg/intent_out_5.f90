! { dg-do run }
!
! PR fortran/41479
!
! Contributed by Juergen Reuter.
!
program main
 type :: container_t
    integer :: n = 42
    ! if the following line is omitted, the problem disappears
    integer, dimension(:), allocatable :: a
 end type container_t

 type(container_t) :: container

 if (container%n /= 42) STOP 1
 if (allocated(container%a)) STOP 2
 container%n = 1
 allocate(container%a(50))
 call init (container)
 if (container%n /= 42) STOP 3
 if (allocated(container%a)) STOP 4
contains
 subroutine init (container)
   type(container_t), intent(out) :: container
 end subroutine init
end program main
