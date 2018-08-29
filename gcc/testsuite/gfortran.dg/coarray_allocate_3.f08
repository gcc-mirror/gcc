! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! Contributed by Ian Harvey  <ian_harvey@bigpond.com>
! Extended by Andre Vehreschild  <vehre@gcc.gnu.org>
! to test that coarray references in allocate work now
! PR fortran/67451

  program main
    implicit none
    type foo
      integer :: bar = 99
    end type
    class(foo), dimension(:), allocatable :: foobar[:]
    class(foo), dimension(:), allocatable :: some_local_object
    allocate(foobar(10)[*])

    allocate(some_local_object, source=foobar)

    if (.not. allocated(foobar)) STOP 1
    if (lbound(foobar, 1) /= 1 .OR. ubound(foobar, 1) /= 10) STOP 2
    if (.not. allocated(some_local_object)) STOP 3
    if (any(some_local_object(:)%bar /= [99, 99,  99, 99, 99, 99, 99, 99, 99, 99])) STOP 4

    deallocate(some_local_object)
    deallocate(foobar)
  end program

