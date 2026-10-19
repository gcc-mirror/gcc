! { dg-do run }
!
! PR 125724
!
! Passing NULL() as an actual argument to an allocatable dummy used to ICE in
! gfc_conv_procedure_call (a gcc_assert that the dummy was optional and not
! allocatable).  The dummy must be received as an unallocated allocatable.

program p
  ! Scalar allocatable, INTENT(IN).
  call s_scalar (null ())

  ! Allocatable array: unallocated on entry, then usable.
  call s_array (null ())

  ! Optional allocatable: NULL() is present-but-unallocated.
  call s_opt (null ())
contains
  subroutine s_scalar (a)
    integer, allocatable, intent(in) :: a
    if (allocated (a)) stop 1
  end subroutine

  subroutine s_array (a)
    integer, allocatable :: a(:)
    if (allocated (a)) stop 2
    allocate (a(3))
    a = [1, 2, 3]
    if (any (a /= [1, 2, 3])) stop 3
    deallocate (a)
  end subroutine

  subroutine s_opt (a)
    integer, allocatable, optional :: a(:)
    if (.not. present (a)) stop 4
    if (allocated (a)) stop 5
  end subroutine
end program p
