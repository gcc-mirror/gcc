! { dg-do compile }
!
! PR fortran/58007
! Unresolved fixup while loading a module.
!
! This tests that the specification expression A%MAX_DEGREE in module BSR is
! correctly loaded and resolved in program MAIN.
!
! Original testcase from Daniel Shapiro <shapero@uw.edu>
! Reduced by Tobias Burnus <burnus@net-b.de> and Janus Weil <janus@gcc.gnu.org>

module matrix
  type :: sparse_matrix
    integer :: max_degree
  end type
contains
  subroutine init_interface (A)
    class(sparse_matrix), intent(in) :: A
  end subroutine
  real function get_value_interface()
  end function
end module

module ellpack
  use matrix
end module

module bsr
  use matrix
  type, extends(sparse_matrix) :: bsr_matrix
  contains
    procedure :: get_neighbors
  end type
contains
  function get_neighbors (A)
    class(bsr_matrix), intent(in) :: A
    integer :: get_neighbors(A%max_degree)
  end function
end module

program main
  use ellpack
  use bsr
end
