! { dg-do run }
! Test the fix for PR38324, in which the bounds were not set correctly for
! constructor assignments with allocatable components.
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>
!
  integer, parameter :: ik4 = 4
  integer, parameter :: ik8 = 8
  integer, parameter :: from = -1, to = 2
  call foo
  call bar
contains
  subroutine foo
    type :: struct
      integer(4), allocatable :: ib(:)
    end type struct
    integer(ik4), allocatable :: ia(:)
    type(struct) :: x
    allocate(ia(from:to))
    if (any(lbound(ia) .ne. -1) .or. any(ubound(ia) .ne. 2)) call abort
    if (any(lbound(ia(:)) .ne. 1) .or. any(ubound(ia(:)) .ne. 4)) call abort
    if (any(lbound(ia(from:to)) .ne. 1) .or. any(ubound(ia(from:to)) .ne. 4)) call abort
    x=struct(ia)
    if (any(lbound(x%ib) .ne. -1) .or. any(ubound(x%ib) .ne. 2)) call abort
    x=struct(ia(:))
    if (any(lbound(x%ib) .ne. 1) .or. any(ubound(x%ib) .ne. 4)) call abort
    x=struct(ia(from:to))
    if (any(lbound(x%ib) .ne. 1) .or. any(ubound(x%ib) .ne. 4)) call abort
    deallocate(ia)
  end subroutine
  subroutine bar
    type :: struct
      integer(4), allocatable :: ib(:)
    end type struct
    integer(ik8), allocatable :: ia(:)
    type(struct) :: x
    allocate(ia(from:to))
    if (any(lbound(ia) .ne. -1) .or. any(ubound(ia) .ne. 2)) call abort
    if (any(lbound(ia(:)) .ne. 1) .or. any(ubound(ia(:)) .ne. 4)) call abort
    if (any(lbound(ia(from:to)) .ne. 1) .or. any(ubound(ia(from:to)) .ne. 4)) call abort
    x=struct(ia)
    if (any(lbound(x%ib) .ne. -1) .or. any(ubound(x%ib) .ne. 2)) call abort
    x=struct(ia(:))
    if (any(lbound(x%ib) .ne. 1) .or. any(ubound(x%ib) .ne. 4)) call abort
    x=struct(ia(from:to))
    if (any(lbound(x%ib) .ne. 1) .or. any(ubound(x%ib) .ne. 4)) call abort
    deallocate(ia)
  end subroutine
end

