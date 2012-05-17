! { dg-do compile }
!
! PR fortran/48889
!
! Thanks for
! reporting to Lawrence Mitchell
! for the test case to David Ham
!
module sparse_tools
  implicit none
  private
  
  type csr_foo
     integer, dimension(:), pointer :: colm=>null()
  end type csr_foo
  
  type block_csr_matrix
     type(csr_foo) :: sparsity
  end type block_csr_matrix

  interface attach_block
     module procedure block_csr_attach_block
  end interface

  interface size
     module procedure  sparsity_size 
  end interface
  
  public :: size, attach_block
contains
  subroutine block_csr_attach_block(matrix, val)
    type(block_csr_matrix), intent(inout) :: matrix
    real, dimension(size(matrix%sparsity%colm)), intent(in), target :: val
  end subroutine block_csr_attach_block

  pure function sparsity_size(sparsity, dim)
    integer :: sparsity_size
    type(csr_foo), intent(in) :: sparsity
    integer, optional, intent(in) :: dim
  end function sparsity_size
end module sparse_tools

module global_numbering
  use sparse_tools
  implicit none
  
  type ele_numbering_type
     integer :: boundaries
  end type ele_numbering_type

  type element_type
     integer :: loc 
     type(ele_numbering_type), pointer :: numbering=>null()
  end type element_type

  type csr_sparsity
  end type csr_sparsity
  
  interface size
     module procedure sparsity_size
  end interface size
contains
  pure function sparsity_size(sparsity, dim)
    integer :: sparsity_size
    type(csr_sparsity), intent(in) :: sparsity
    integer, optional, intent(in) :: dim
  end function sparsity_size

  subroutine make_boundary_numbering(EEList, xndglno, ele_n)
    type(csr_sparsity), intent(in) :: EEList
    type(element_type), intent(in) :: ele_n
    integer, dimension(size(EEList,1)*ele_n%loc), intent(in), target ::&
         & xndglno 
    integer, dimension(ele_n%numbering%boundaries) :: neigh
    integer :: j
    j=size(neigh)
  end subroutine make_boundary_numbering
end module global_numbering

module sparse_matrices_fields
  use sparse_tools
implicit none
   type scalar_field
      real, dimension(:), pointer :: val
   end type scalar_field
contains  
  subroutine csr_mult_T_scalar(x)
    type(scalar_field), intent(inout) :: x
    real, dimension(:), allocatable :: tmp
    integer :: i
    i=size(x%val)
  end subroutine csr_mult_T_scalar
end module sparse_matrices_fields

program test
  use sparse_matrices_fields
  use global_numbering
end program test
