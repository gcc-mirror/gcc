! { dg-do run }
!
! PR fortran/92178
! Contributed by Tobias Burnus

program foo
  implicit none (type, external)

  type t
  end type t

  type, extends(t) :: t2
  end type t2

  type(t2) :: x2
  class(t), allocatable :: aa

  call check_intentout_false(allocated(aa), aa, &
                             allocated(aa))
  if (allocated(aa)) stop 1

  allocate(t2 :: aa)
  if (.not.allocated(aa)) stop 2
  if (.not.same_type_as(aa, x2)) stop 3
  call check_intentout_true(allocated(aa), (same_type_as(aa, x2)), aa, &
                            allocated(aa), (same_type_as(aa, x2)))
  if (allocated(aa)) stop 4

contains
  subroutine check_intentout_false(alloc1, yy, alloc2)
    logical, value :: alloc1, alloc2
    class(t), allocatable, intent(out) :: yy
    if (allocated(yy)) stop 11
    if (alloc1) stop 12
    if (alloc2) stop 13
  end subroutine check_intentout_false
  subroutine check_intentout_true(alloc1, same1, zz, alloc2, same2)
    logical, value :: alloc1, alloc2, same1, same2
    class(t), allocatable, intent(out) :: zz
    if (allocated(zz)) stop 21
    if (.not.alloc1) stop 22
    if (.not.alloc2) stop 23
    if (.not.same1) stop 24
    if (.not.same2) stop 25
  end subroutine check_intentout_true
end program
