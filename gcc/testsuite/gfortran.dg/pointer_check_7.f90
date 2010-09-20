! { dg-do compile }
! { dg-options "-fcheck=pointer" }
!
! PR 45438: [4.6 Regression] [OOP] ICE with -fcheck=pointer
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

module base_mat_mod

  implicit none 

  type  :: base_sparse_mat
  contains 
    procedure :: get_fmt
  end type

contains

  function get_fmt(a) result(res)
    class(base_sparse_mat), intent(in) :: a
    character(len=5) :: res
    res = 'NULL'
  end function

  subroutine errlog(name)
    character(len=*) :: name
  end subroutine

  subroutine test (a)
    class(base_sparse_mat), intent(in) :: a
    call errlog(a%get_fmt())
  end subroutine

end module

! { dg-final { cleanup-modules "base_mat_mod" } }
