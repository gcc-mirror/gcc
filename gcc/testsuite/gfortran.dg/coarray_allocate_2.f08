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
    class(foo), allocatable :: foobar[:]
    class(foo), allocatable :: some_local_object
    allocate(foobar[*])

    allocate(some_local_object, source=foobar)

    if (.not. allocated(foobar)) STOP 1
    if (.not. allocated(some_local_object)) STOP 2

    deallocate(some_local_object)
    deallocate(foobar)
  end program

