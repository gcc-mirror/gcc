! { dg-do run }
! Test the fix for PR43895, in which the dummy 'a' was not
! dereferenced for the deallocation of component 'a', as required
! for INTENT(OUT).
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>
!
module d_mat_mod
  type  :: base_sparse_mat
  end type base_sparse_mat

  type, extends(base_sparse_mat) :: d_base_sparse_mat
    integer :: i
  end type d_base_sparse_mat

  type :: d_sparse_mat
    class(d_base_sparse_mat), allocatable  :: a 
  end type d_sparse_mat
end module d_mat_mod

  use d_mat_mod
  type(d_sparse_mat) :: b
  allocate (b%a)
  b%a%i = 42
  call bug14 (b)
  if (allocated (b%a)) call abort
contains
  subroutine bug14(a)
    implicit none
    type(d_sparse_mat), intent(out) :: a
  end subroutine bug14
end
