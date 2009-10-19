! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR 41586: Allocatable _scalars_ are never auto-deallocated
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module automatic_deallocation

  type t0
    integer :: i
  end type

  type t1
    real :: pi = 3.14
    integer, allocatable :: j
  end type

  type t2
    class(t0), allocatable :: k
  end type t2

contains

  ! (1) simple allocatable scalars
  subroutine a
    integer, allocatable :: m
    allocate (m)
    m = 42
  end subroutine

  ! (2) allocatable scalar CLASS variables
  subroutine b
    class(t0), allocatable :: m
    allocate (t0 :: m)
    m%i = 43
  end subroutine

  ! (3) allocatable scalar components
  subroutine c
    type(t1) :: m
    allocate (m%j)
    m%j = 44
  end subroutine

  ! (4) allocatable scalar CLASS components
  subroutine d
    type(t2) :: m
    allocate (t0 :: m%k)
    m%k%i = 45
  end subroutine

end module 


! { dg-final { scan-tree-dump-times "__builtin_free" 5 "original" } }

! { dg-final { cleanup-modules "automatic_deallocation" } }
! { dg-final { cleanup-tree-dump "original" } }
