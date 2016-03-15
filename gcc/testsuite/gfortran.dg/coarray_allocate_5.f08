! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single -fdump-tree-original" }
! { dg-additional-options "-latomic" { target libatomic_available } }
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

    if (.not. allocated(foobar)) call abort()
    if (lbound(foobar, 1) /= 1 .OR. ubound(foobar, 1) /= 10) call abort()
    if (.not. allocated(some_local_object)) call abort()
    if (any(some_local_object(:)%bar /= [99, 99,  99, 99, 99, 99, 99, 99, 99, 99])) call abort()

    deallocate(some_local_object)
    deallocate(foobar)
  end program

! Check that some_local_object is treated as rank-1 array.
! This failed beforehand, because the coarray attribute of the source=expression
! was propagated to some_local_object in the allocate.
! { dg-final { scan-tree-dump-not "some_local_object\._data\.dim\[1\]\.lbound" "original" } }
