! { dg-do compile }
! Tests the fix for PR4164656 in which the call to a%a%scal failed to compile.
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>
!
module const_mod
  integer, parameter  :: longndig=12
  integer, parameter  :: long_int_k_ = selected_int_kind(longndig)
  integer, parameter  :: dpk_ = kind(1.d0)
  integer, parameter  :: spk_ = kind(1.e0)
end module const_mod

module base_mat_mod  
  use const_mod 
  type  :: base_sparse_mat
    integer, private     :: m, n
    integer, private     :: state, duplicate 
    logical, private     :: triangle, unitd, upper, sorted
  contains 
    procedure, pass(a) :: get_nzeros
  end type base_sparse_mat
  private ::  get_nzeros
contains
  function get_nzeros(a) result(res)
    implicit none 
    class(base_sparse_mat), intent(in) :: a
    integer :: res
    integer :: err_act
    character(len=20)  :: name='base_get_nzeros'
    logical, parameter :: debug=.false.
    res = -1
  end function get_nzeros
end module base_mat_mod

module s_base_mat_mod
  use base_mat_mod
  type, extends(base_sparse_mat) :: s_base_sparse_mat
  contains
    procedure, pass(a) :: s_scals
    procedure, pass(a) :: s_scal
    generic, public    :: scal => s_scals, s_scal 
  end type s_base_sparse_mat
  private :: s_scals, s_scal

  type, extends(s_base_sparse_mat) :: s_coo_sparse_mat
    
    integer              :: nnz
    integer, allocatable :: ia(:), ja(:)
    real(spk_), allocatable :: val(:)
  contains
    procedure, pass(a) :: get_nzeros => s_coo_get_nzeros
    procedure, pass(a) :: s_scals => s_coo_scals
    procedure, pass(a) :: s_scal => s_coo_scal
  end type s_coo_sparse_mat
  private :: s_coo_scals, s_coo_scal, s_coo_get_nzeros
contains 
  subroutine s_scals(d,a,info) 
    implicit none 
    class(s_base_sparse_mat), intent(in) :: a
    real(spk_), intent(in)      :: d
    integer, intent(out)            :: info

    Integer :: err_act
    character(len=20)  :: name='s_scals'
    logical, parameter :: debug=.false.

    ! This is the base version. If we get here
    ! it means the derived class is incomplete,
    ! so we throw an error.
    info = 700
  end subroutine s_scals


  subroutine s_scal(d,a,info) 
    implicit none 
    class(s_base_sparse_mat), intent(in) :: a
    real(spk_), intent(in)      :: d(:)
    integer, intent(out)            :: info

    Integer :: err_act
    character(len=20)  :: name='s_scal'
    logical, parameter :: debug=.false.

    ! This is the base version. If we get here
    ! it means the derived class is incomplete,
    ! so we throw an error.
    info = 700
  end subroutine s_scal

  function s_coo_get_nzeros(a) result(res)
    implicit none 
    class(s_coo_sparse_mat), intent(in) :: a
    integer :: res
    res  = a%nnz
  end function s_coo_get_nzeros


  subroutine s_coo_scal(d,a,info) 
    use const_mod
    implicit none 
    class(s_coo_sparse_mat), intent(inout) :: a
    real(spk_), intent(in)      :: d(:)
    integer, intent(out)            :: info

    Integer :: err_act,mnm, i, j, m
    character(len=20)  :: name='scal'
    logical, parameter :: debug=.false.
    info  = 0
    do i=1,a%get_nzeros()
      j        = a%ia(i)
      a%val(i) = a%val(i) * d(j)
    enddo
  end subroutine s_coo_scal

  subroutine s_coo_scals(d,a,info) 
    use const_mod
    implicit none 
    class(s_coo_sparse_mat), intent(inout) :: a
    real(spk_), intent(in)      :: d
    integer, intent(out)            :: info

    Integer :: err_act,mnm, i, j, m
    character(len=20)  :: name='scal'
    logical, parameter :: debug=.false.

    info  = 0
    do i=1,a%get_nzeros()
      a%val(i) = a%val(i) * d
    enddo
  end subroutine s_coo_scals
end module s_base_mat_mod

module s_mat_mod
  use s_base_mat_mod
  type :: s_sparse_mat
    class(s_base_sparse_mat), pointer  :: a
  contains
    procedure, pass(a) :: s_scals
    procedure, pass(a) :: s_scal
    generic, public    :: scal => s_scals, s_scal 
  end type s_sparse_mat
  interface scal
    module procedure s_scals, s_scal
  end interface
contains 
  subroutine s_scal(d,a,info)
    use const_mod
    implicit none 
    class(s_sparse_mat), intent(inout) :: a
    real(spk_), intent(in)              :: d(:)
    integer, intent(out)                    :: info
    integer :: err_act
    character(len=20)  :: name='csnmi'
    logical, parameter :: debug=.false.
    print *, "s_scal"
    call a%a%scal(d,info)
    return
  end subroutine s_scal

  subroutine s_scals(d,a,info)
    use const_mod
    implicit none 
    class(s_sparse_mat), intent(inout) :: a
    real(spk_), intent(in)              :: d
    integer, intent(out)                    :: info
    integer :: err_act
    character(len=20)  :: name='csnmi'
    logical, parameter :: debug=.false.
    print *, "s_scals"
    call a%a%scal(d,info)
    return
  end subroutine s_scals
end module s_mat_mod

    use s_mat_mod
    class (s_sparse_mat), pointer :: a
    type (s_sparse_mat), target :: b
    type (s_base_sparse_mat), target :: c
    integer info
    b%a => c
    a => b
    call a%scal (1.0_spk_, info)
end
! { dg-final { cleanup-modules "const_mod base_mat_mod s_base_mat_mod s_mat_mod" } }

