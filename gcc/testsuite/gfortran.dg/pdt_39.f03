! { dg-do run }
!
! Test the fix for pr95541.
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module mykinds 
  use, intrinsic :: iso_fortran_env, only : i4 => int32, r4 => real32, r8 => real64 
  implicit none
  private
  public :: i4, r4, r8
end module mykinds
 
module matrix
  use mykinds, only : r4, r8
  implicit none
  private
 
  type, public :: mat_t(k,c,r)
     !.. type parameters
     integer, kind :: k = r4
     integer, len :: c = 1
     integer, len :: r = 1
     private
     !.. private by default
     !.. type data
     real(kind=k) :: m_a(c,r)
  end type mat_t
 
  interface assignment(=)
     module procedure geta_r4
     module procedure seta_r4
     module procedure geta_r8
     module procedure seta_r8
     !.. additional bindings elided
  end interface assignment(=)
  
  public :: assignment(=)
  
contains
  
  subroutine geta_r4(a_lhs, t_rhs)   
    real(r4), allocatable, intent(out) :: a_lhs(:,:)
    class(mat_t(k=r4,c=*,r=*)), intent(in) :: t_rhs   
    a_lhs = t_rhs%m_a
    return
  end subroutine geta_r4
 
  subroutine geta_r8(a_lhs, t_rhs)
    real(r8), allocatable, intent(out) :: a_lhs(:,:)
    class(mat_t(k=r8,c=*,r=*)), intent(in) :: t_rhs
    a_lhs = t_rhs%m_a                   ! Leaks 152 bytes in 2 blocks
    return 
  end subroutine geta_r8
 
  subroutine seta_r4(t_lhs, a_rhs) 
    class(mat_t(k=r4,c=*,r=*)), intent(inout) :: t_lhs
    real(r4), intent(in) :: a_rhs(:,:)
    !.. checks on size elided
    t_lhs%m_a = a_rhs
    return 
  end subroutine seta_r4
 
  subroutine seta_r8(t_lhs, a_rhs) 
    class(mat_t(k=r8,c=*,r=*)), intent(inout) :: t_lhs
    real(r8), intent(in) :: a_rhs(:,:)
    !.. checks on size elided
    t_lhs%m_a = a_rhs
    return 
  end subroutine seta_r8
 
end module matrix

program p  
  use mykinds, only : r4, r8
  use matrix, only : mat_t, assignment(=)  
  implicit none
  type(mat_t(k=r4,c=:,r=:)), allocatable :: mat_r4
  type(mat_t(k=r8,c=:,r=:)), allocatable :: mat_r8
  real(r4), allocatable :: a_r4(:,:)
  real(r8), allocatable :: a_r8(:,:)
  integer :: N
  integer :: M
  integer :: i
  integer :: istat
  N = 2
  M = 3
  allocate( mat_t(k=r4,c=N,r=M) :: mat_r4, stat=istat )
  if ( istat /= 0 ) then
     print *, " error allocating mat_r4: stat = ", istat
     stop
  end if
  if (mat_r4%k /= r4) stop 1
  if (mat_r4%c /= N) stop 2
  if (mat_r4%r /= M) stop 3
  mat_r4 = reshape( [ (real(i, kind=mat_r4%k), i=1,N*M) ], [ N, M ] )
  a_r4 = mat_r4                         ! Leaks 24 bytes in 1 block.
  if (int (sum (a_r4)) /= 21) stop 4
  N = 4
  M = 4
  allocate( mat_t(k=r8,c=N,r=M) :: mat_r8, stat=istat )
  if ( istat /= 0 ) then
     print *, " error allocating mat_r4: stat = ", istat
     stop
  end if
  if (mat_r8%k /= r8) stop 5
  if (mat_r8%c /= N) stop 6
  if (mat_r8%r /= M) stop 7
  mat_r8 = reshape( [ (real(i, kind=mat_r8%k), i=1,N*M) ], [ N, M ] )
  a_r8 = mat_r8
  if (int (sum (a_r8)) /= 136) stop 8
  deallocate( mat_r4, stat=istat )
  if ( istat /= 0 ) then
     print *, " error deallocating mat_r4: stat = ", istat
     stop
  end if
  deallocate( mat_r8, stat=istat )
  if ( istat /= 0 ) then
     print *, " error deallocating mat_r4: stat = ", istat
     stop
  end if
  stop
end program p
