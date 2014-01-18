! { dg-do compile }
!
! PR fortran/58007
! Unresolved fiixup while loading a module.
!
! This tests that the specification expression A%MAX_DEGREE in module BSR is
! correctly loaded and resolved in program MAIN.
!
! Original testcase from Daniel Shapiro <shapero@uw.edu>

module matrix
  type :: sparse_matrix
    integer :: max_degree
  end type
end module

module bsr
  use matrix

  type, extends(sparse_matrix) :: bsr_matrix
  end type

  integer :: i1
  integer :: i2
  integer :: i3
contains
  function get_neighbors (A)
    type(bsr_matrix), intent(in) :: A
    integer :: get_neighbors(A%max_degree)
  end function
end module

program main
  use matrix
  use bsr
end
