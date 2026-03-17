! { dg-do compile }
! PR fortran/124235 - ICE in ALLOCATE of sub-objects with recursive types
!
! Mutually-referencing derived types with a mix of allocatable and
! fixed-size array components.  Allocating a sub-object of an
! already-allocated component triggered a segfault during tree lowering
! because garbage collection during deep-copy wrapper generation
! invalidated locally-computed COMPONENT_REF tree nodes.

program pr124235
  implicit none

  type :: alpha_t
    integer, allocatable :: vals(:)
    type(alpha_t), allocatable :: a_kids(:)
    type(gamma_t), allocatable :: g_ref(:)
    integer :: tag
    type(beta_t), allocatable :: b_ref(:)
  end type

  type :: beta_t
    integer, allocatable :: vals(:)
    type(alpha_t) :: a_fixed(4)
    type(beta_t), allocatable :: b_kids(:)
    integer :: tag
    type(gamma_t), allocatable :: g_ref(:)
  end type

  type :: gamma_t
    integer, allocatable :: vals(:)
    type(beta_t) :: b_fixed(4)
    integer :: tag
    type(gamma_t), allocatable :: g_kids(:)
  end type

  type :: container_t
    type(gamma_t), allocatable :: items(:)
    type(gamma_t), allocatable :: spares(:)
  end type

  type(container_t) :: box

  allocate(box%items(6))
  allocate(box%items(2)%g_kids(3))

end program
