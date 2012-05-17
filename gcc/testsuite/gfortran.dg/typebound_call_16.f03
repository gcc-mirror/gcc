! { dg-do compile }
!
! PR 41685: [OOP] internal compiler error: verify_flow_info failed
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

module base_mat_mod

  type  :: base_sparse_mat
  contains 
    procedure, pass(a) :: get_nrows
  end type base_sparse_mat
  
contains

  integer function get_nrows(a)
    implicit none 
    class(base_sparse_mat), intent(in) :: a
  end function get_nrows

end module  base_mat_mod


  use base_mat_mod

  type, extends(base_sparse_mat) :: s_coo_sparse_mat
  end type s_coo_sparse_mat

  class(s_coo_sparse_mat), pointer :: a
  Integer :: m
  m = a%get_nrows()

end
