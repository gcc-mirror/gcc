! { dg-do compile }
!
! PR fortran/45125
!
! Contributed by Salvatore Filippone and Dominique d'Humieres.
!

module const_mod
  ! This is the default integer
  integer, parameter  :: ndig=8
  integer, parameter  :: int_k_ = selected_int_kind(ndig)
  ! This is an 8-byte  integer, and normally different from default integer. 
  integer, parameter  :: longndig=12
  integer, parameter  :: long_int_k_ = selected_int_kind(longndig)
  !
  ! These must be the kind parameter corresponding to MPI_DOUBLE_PRECISION
  ! and MPI_REAL
  !
  integer, parameter  :: dpk_ = kind(1.d0)
  integer, parameter  :: spk_ = kind(1.e0)
  integer, save       :: sizeof_dp, sizeof_sp
  integer, save       :: sizeof_int, sizeof_long_int
  integer, save       :: mpi_integer

  integer, parameter :: invalid_ = -1 
  integer, parameter :: spmat_null_=0, spmat_bld_=1
  integer, parameter :: spmat_asb_=2, spmat_upd_=4

  !
  ! 
  !     Error constants
  integer, parameter, public :: success_=0
  integer, parameter, public :: err_iarg_neg_=10
end module const_mod
module base_mat_mod
  
  use const_mod 


  type  :: base_sparse_mat
    integer, private     :: m, n
    integer, private     :: state, duplicate 
    logical, private     :: triangle, unitd, upper, sorted
  contains 

    procedure, pass(a) :: get_fmt => base_get_fmt
    procedure, pass(a) :: set_null => base_set_null
    procedure, pass(a) :: allocate_mnnz => base_allocate_mnnz
    generic,   public  :: allocate => allocate_mnnz
  end type base_sparse_mat

  interface 
    subroutine  base_allocate_mnnz(m,n,a,nz) 
      import base_sparse_mat, long_int_k_
      integer, intent(in) :: m,n
      class(base_sparse_mat), intent(inout) :: a
      integer, intent(in), optional  :: nz
    end subroutine base_allocate_mnnz
  end interface

contains

  function base_get_fmt(a) result(res)
    implicit none 
    class(base_sparse_mat), intent(in) :: a
    character(len=5) :: res
    res = 'NULL'
  end function base_get_fmt

  subroutine  base_set_null(a) 
    implicit none 
    class(base_sparse_mat), intent(inout) :: a

    a%state = spmat_null_
  end subroutine base_set_null


end module base_mat_mod

module d_base_mat_mod
  
  use base_mat_mod

  type, extends(base_sparse_mat) :: d_base_sparse_mat
  contains
  end type d_base_sparse_mat
  
  
  
  type, extends(d_base_sparse_mat) :: d_coo_sparse_mat
    
    integer              :: nnz
    integer, allocatable :: ia(:), ja(:)
    real(dpk_), allocatable :: val(:)
    
  contains
    
    procedure, pass(a) :: get_fmt      => d_coo_get_fmt
    procedure, pass(a) :: allocate_mnnz => d_coo_allocate_mnnz
    
  end type d_coo_sparse_mat
  
  
  interface
    subroutine  d_coo_allocate_mnnz(m,n,a,nz) 
      import d_coo_sparse_mat
      integer, intent(in) :: m,n
      class(d_coo_sparse_mat), intent(inout) :: a
      integer, intent(in), optional :: nz
    end subroutine d_coo_allocate_mnnz
  end interface
  
contains 
  
  function d_coo_get_fmt(a) result(res)
    implicit none 
    class(d_coo_sparse_mat), intent(in) :: a
    character(len=5) :: res
    res = 'COO'
  end function d_coo_get_fmt
  
end module d_base_mat_mod

subroutine  base_allocate_mnnz(m,n,a,nz) 
  use base_mat_mod, protect_name => base_allocate_mnnz
  implicit none 
  integer, intent(in) :: m,n
  class(base_sparse_mat), intent(inout) :: a
  integer, intent(in), optional  :: nz
  Integer :: err_act
  character(len=20)  :: name='allocate_mnz', errfmt
  logical, parameter :: debug=.false.

  ! This is the base version. If we get here
  ! it means the derived class is incomplete,
  ! so we throw an error.
  errfmt=a%get_fmt()
  write(0,*) 'Error: Missing ovverriding impl for allocate in class ',errfmt

  return

end subroutine base_allocate_mnnz

subroutine  d_coo_allocate_mnnz(m,n,a,nz) 
  use d_base_mat_mod, protect_name => d_coo_allocate_mnnz
  implicit none 
  integer, intent(in) :: m,n
  class(d_coo_sparse_mat), intent(inout) :: a
  integer, intent(in), optional :: nz
  Integer :: err_act, info, nz_
  character(len=20)  :: name='allocate_mnz'
  logical, parameter :: debug=.false.

  info = success_
  if (m < 0) then 
    info = err_iarg_neg_
  endif
  if (n < 0) then 
    info = err_iarg_neg_
  endif
  if (present(nz)) then 
    nz_ = nz
  else
    nz_ = max(7*m,7*n,1)
  end if
  if (nz_ < 0) then 
    info = err_iarg_neg_
  endif
! !$  if (info == success_) call realloc(nz_,a%ia,info)
! !$  if (info == success_) call realloc(nz_,a%ja,info)
! !$  if (info == success_) call realloc(nz_,a%val,info)
  if (info == success_) then 
! !$    call a%set_nrows(m)
! !$    call a%set_ncols(n)
! !$    call a%set_nzeros(0)
! !$    call a%set_bld()
! !$    call a%set_triangle(.false.)
! !$    call a%set_unit(.false.)
! !$    call a%set_dupl(dupl_def_)
    write(0,*) 'Allocated COO succesfully, should now set components'
  else 
    write(0,*) 'COO allocation failed somehow. Go figure'
  end if
  return

end subroutine d_coo_allocate_mnnz


program d_coo_err
  use d_base_mat_mod
  implicit none

  integer            :: ictxt, iam, np

  ! solver parameters
  type(d_coo_sparse_mat) :: acoo
  
  ! other variables
  integer nnz, n

  n   = 32
  nnz = n*9
  
  call acoo%set_null()
  call acoo%allocate(n,n,nz=nnz)

  stop
end program d_coo_err

! { dg-final { cleanup-modules "base_mat_mod const_mod d_base_mat_mod" } }
