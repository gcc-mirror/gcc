! { dg-do compile }
!
! PR 45439: [OOP] SELECT TYPE bogus complaint about INTENT
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>


module d_base_mat_mod

  implicit none

  type :: d_base_sparse_mat
  contains
    procedure, pass(a) :: mv_to_coo   => d_base_mv_to_coo   
  end type d_base_sparse_mat

  interface 
    subroutine d_base_mv_to_coo(a)
      import d_base_sparse_mat
      class(d_base_sparse_mat), intent(inout) :: a
    end subroutine d_base_mv_to_coo
  end interface

  type :: d_sparse_mat
    class(d_base_sparse_mat), allocatable  :: a 
  end type d_sparse_mat

contains

  subroutine bug21(ax)
    type(d_sparse_mat), intent(inout) :: ax
    select type(aa=> ax%a)
    class default
      call aa%mv_to_coo() 
    end select
  end subroutine bug21

end module d_base_mat_mod


! { dg-final { cleanup-modules "d_base_mat_mod" } }
