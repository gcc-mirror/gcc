! { dg-do run }
! { dg-additional-options "-fcheck=bounds" }
!
! PR fortran/104819 - passing NULL() to assumed-rank, derived type dummy

program null_actual
  implicit none
  integer :: stop_base
  type t
  end type t
  type(t), pointer     :: p2(:,:) => NULL()
  type(t), allocatable :: a2(:,:)
  type(t), pointer     :: p0 => NULL ()
  type(t), allocatable :: a0

  ! Basic tests passing unallocated allocatable / disassociated pointer
  stop_base = 0
  ! ... to assumed-rank dummy:
  call chk_t_a (a2)
  call chk_t_p (p2)
  call chk_t_a_i (a2)
  call chk_t_p_i (p2)
  call opt_t_a (a2)
  call opt_t_p (p2)
  call opt_t_a_i (a2)
  call opt_t_p_i (p2)
  ! ... to rank-2 dummy:
  call chk2_t_a (a2)
  call chk2_t_p (p2)
  call opt2_t_a (a2)
  call opt2_t_p (p2)
  ! ... to rank-0 dummy:
  stop_base = 60
  call chk0_t_a (a0)
  call chk0_t_p (p0)
  call opt0_t_a (a0)
  call opt0_t_p (p0)
  call chk0_t_a_i (a0)
  call chk0_t_p_i (p0)
  call opt0_t_a_i (a0)
  call opt0_t_p_i (p0)

  ! Test NULL with MOLD argument
  stop_base = 20
  call chk_t_a (null(a2))
  call chk_t_p (null(p2))
  call chk_t_a_i (null(a2))
  call chk_t_p_i (null(p2))
  call opt_t_a (null(a2))
  call opt_t_p (null(p2))
  call opt_t_a_i (null(a2))
  call opt_t_p_i (null(p2))
  call chk2_t_a (null(a2))
  call chk2_t_p (null(p2))
  call opt2_t_a (null(a2))
  call opt2_t_p (null(p2))

  stop_base = 80
  call chk0_t_a (null(a0))
  call chk0_t_p (null(p0))
  call opt0_t_a (null(a0))
  call opt0_t_p (null(p0))
  call chk0_t_a_i (null(a0))
  call chk0_t_p_i (null(p0))
  call opt0_t_a_i (null(a0))
  call opt0_t_p_i (null(p0))

  ! Test NULL without MOLD argument
  stop_base = 40
  call chk2_t_a (null())
  call chk2_t_p (null())
  call opt2_t_a (null())
  call opt2_t_p (null())

  stop_base = 100
  call chk0_t_a (null())
  call chk0_t_p (null())
  call opt0_t_a (null())
  call opt0_t_p (null())
  call chk0_t_a_i (null())
  call chk0_t_p_i (null())
  call opt0_t_a_i (null())
  call opt0_t_p_i (null())

contains
  ! Check assumed-rank dummy:
  subroutine chk_t_a (x)
    type(t), allocatable :: x(..)
    if (rank (x) /= 2) stop stop_base + 1
    if (allocated (x)) stop stop_base + 2
  end subroutine chk_t_a

  subroutine chk_t_a_i (x)
    type(t), allocatable, intent(in) :: x(..)
    if (rank (x) /= 2) stop stop_base + 3
    if (allocated (x)) stop stop_base + 4
  end subroutine chk_t_a_i

  subroutine chk_t_p (x)
    type(t), pointer :: x(..)
    if (rank (x) /= 2)  stop stop_base + 5
    if (associated (x)) stop stop_base + 6
  end subroutine chk_t_p

  subroutine chk_t_p_i (x)
    type(t), pointer, intent(in) :: x(..)
    if (rank (x) /= 2)  stop stop_base + 7
    if (associated (x)) stop stop_base + 8
  end subroutine chk_t_p_i

  ! Check assumed-rank optional dummy:
  subroutine opt_t_a (x)
    type(t), optional, allocatable :: x(..)
    if (.not. present (x)) stop stop_base + 11
  end subroutine opt_t_a

  subroutine opt_t_a_i (x)
    type(t), optional, allocatable, intent(in) :: x(..)
    if (.not. present (x)) stop stop_base + 12
  end subroutine opt_t_a_i

  subroutine opt_t_p (x)
    type(t), optional, pointer :: x(..)
    if (.not. present (x)) stop stop_base + 13
  end subroutine opt_t_p

  subroutine opt_t_p_i (x)
    type(t), optional, pointer, intent(in) :: x(..)
    if (.not. present (x)) stop stop_base + 14
  end subroutine opt_t_p_i

  ! Checks with fixed rank:
  subroutine chk2_t_a (x)
    type(t), allocatable :: x(:,:)
    if (allocated (x)) stop stop_base + 15
  end subroutine chk2_t_a

  subroutine chk2_t_p (x)
    type(t), pointer, intent(in) :: x(:,:)
    if (associated (x)) stop stop_base + 16
  end subroutine chk2_t_p

  ! Checks with fixed rank optional dummy:
  subroutine opt2_t_a (x)
    type(t), optional, allocatable :: x(:,:)
    if (.not. present (x)) stop stop_base + 17
    if (allocated (x))     stop stop_base + 18
  end subroutine opt2_t_a

  subroutine opt2_t_p (x)
    type(t), optional, pointer, intent(in) :: x(:,:)
    if (.not. present (x)) stop stop_base + 19
    if (associated (x))    stop stop_base + 20
  end subroutine opt2_t_p

  ! Checks for rank-0 dummy:
  subroutine chk0_t_p (x)
    type(t), pointer :: x
    if (associated (x)) stop stop_base + 1
  end subroutine chk0_t_p

  subroutine chk0_t_p_i (x)
    type(t), pointer, intent(in) :: x
    if (associated (x)) stop stop_base + 2
  end subroutine chk0_t_p_i

  subroutine opt0_t_p (x)
    type(t), pointer, optional :: x
    if (.not. present (x)) stop stop_base + 3
    if (associated (x))    stop stop_base + 4
  end subroutine opt0_t_p

  subroutine opt0_t_p_i (x)
    type(t), pointer, optional, intent(in) :: x
    if (.not. present (x)) stop stop_base + 5
    if (associated (x))    stop stop_base + 6
  end subroutine opt0_t_p_i

  subroutine chk0_t_a (x)
    type(t), allocatable :: x
    if (allocated (x)) stop stop_base + 7
  end subroutine chk0_t_a

  subroutine chk0_t_a_i (x)
    type(t), allocatable, intent(in) :: x
    if (allocated (x)) stop stop_base + 8
  end subroutine chk0_t_a_i

  subroutine opt0_t_a (x)
    type(t), allocatable, optional :: x
    if (.not. present (x)) stop stop_base +  9
    if (allocated (x))     stop stop_base + 10
  end subroutine opt0_t_a

  subroutine opt0_t_a_i (x)
    type(t), allocatable, optional, intent(in) :: x
    if (.not. present (x)) stop stop_base + 11
    if (allocated (x))     stop stop_base + 12
  end subroutine opt0_t_a_i
end
