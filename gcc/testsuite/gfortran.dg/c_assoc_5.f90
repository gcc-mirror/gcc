! { dg-do compile }
!
! PR fortran/56969
!
! Contributed by Salvatore Filippone
!
! Was before rejected as the different c_associated weren't recognized to
! come from the same module.
!
module test_mod
  use iso_c_binding 

  type(c_ptr), save :: test_context = c_null_ptr

  type, bind(c) :: s_Cmat
    type(c_ptr) :: Mat = c_null_ptr
  end type s_Cmat

  
  interface 
    function FtestCreate(context) &
         & bind(c,name="FtestCreate") result(res)
      use iso_c_binding
      type(c_ptr)    :: context
      integer(c_int) :: res
    end function FtestCreate
  end interface
contains
  
  function initFtest() result(res)
    implicit none 
    integer(c_int) :: res
    if (c_associated(test_context)) then 
      res = 0
    else
      res = FtestCreate(test_context)
    end if
  end function initFtest
end module test_mod

module base_mat_mod
  type base_sparse_mat
    integer, allocatable :: ia(:)
  end type base_sparse_mat
end module base_mat_mod

module extd_mat_mod

  use iso_c_binding
  use test_mod
  use base_mat_mod

  type, extends(base_sparse_mat) :: extd_sparse_mat
    type(s_Cmat) :: deviceMat
  end type extd_sparse_mat

end module extd_mat_mod

subroutine extd_foo(a) 

  use extd_mat_mod
  implicit none 
  class(extd_sparse_mat), intent(inout) :: a

  if (c_associated(a%deviceMat%Mat)) then 
    write(*,*) 'C Associated'
  end if

end subroutine extd_foo
