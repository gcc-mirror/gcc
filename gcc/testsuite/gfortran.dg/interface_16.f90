! { dg-do compile }
! This tests the fix for PR32634, in which the generic interface
! in foo_pr_mod was given the original rather than the local name.
! This meant that the original name had to be used in the calll
! in foo_sub.
!
! Contributed by Salvatore Filippone <salvatore.filippone@uniroma2.it>

module foo_base_mod
  type foo_dmt
    real(kind(1.d0)), allocatable  :: rv(:)
    integer, allocatable :: iv1(:), iv2(:)
  end type foo_dmt
  type foo_zmt
    complex(kind(1.d0)), allocatable  :: rv(:)
    integer, allocatable  :: iv1(:), iv2(:)
  end type foo_zmt
  type foo_cdt
     integer, allocatable :: md(:)
     integer, allocatable :: hi(:), ei(:)
  end type foo_cdt
end module foo_base_mod

module bar_prt
  use foo_base_mod, only : foo_dmt, foo_zmt, foo_cdt
  type bar_dbprt
    type(foo_dmt), allocatable :: av(:) 
    real(kind(1.d0)), allocatable      :: d(:)  
    type(foo_cdt)                :: cd 
  end type bar_dbprt
  type bar_dprt
    type(bar_dbprt), allocatable  :: bpv(:) 
  end type bar_dprt
  type bar_zbprt
    type(foo_zmt), allocatable :: av(:) 
    complex(kind(1.d0)), allocatable   :: d(:)  
    type(foo_cdt)                :: cd 
  end type bar_zbprt
  type bar_zprt
    type(bar_zbprt), allocatable  :: bpv(:) 
  end type bar_zprt
end module bar_prt

module bar_pr_mod
  use bar_prt
  interface bar_pwrk
    subroutine bar_dppwrk(pr,x,y,cd,info,trans,work)
      use foo_base_mod
      use bar_prt
      type(foo_cdt),intent(in)    :: cd
      type(bar_dprt), intent(in)  :: pr
      real(kind(0.d0)),intent(inout)    :: x(:), y(:)
      integer, intent(out)              :: info
      character(len=1), optional        :: trans
      real(kind(0.d0)),intent(inout), optional, target :: work(:)
    end subroutine bar_dppwrk
    subroutine bar_zppwrk(pr,x,y,cd,info,trans,work)
      use foo_base_mod
      use bar_prt
      type(foo_cdt),intent(in)    :: cd
      type(bar_zprt), intent(in)  :: pr
      complex(kind(0.d0)),intent(inout) :: x(:), y(:)
      integer, intent(out)              :: info
      character(len=1), optional        :: trans
      complex(kind(0.d0)),intent(inout), optional, target :: work(:)
    end subroutine bar_zppwrk
  end interface
end module bar_pr_mod

module foo_pr_mod
  use bar_prt, &
       & foo_dbprt  => bar_dbprt,&
       & foo_zbprt  => bar_zbprt,&
       & foo_dprt   => bar_dprt,&
       & foo_zprt   => bar_zprt 
  use bar_pr_mod, &
       & foo_pwrk  => bar_pwrk
end module foo_pr_mod

Subroutine foo_sub(a,pr,b,x,eps,cd,info)
  use foo_base_mod
  use foo_pr_mod
  Implicit None
!!$  parameters 
  Type(foo_dmt), Intent(in)  :: a
  Type(foo_dprt), Intent(in)   :: pr 
  Type(foo_cdt), Intent(in)    :: cd
  Real(Kind(1.d0)), Intent(in)       :: b(:)
  Real(Kind(1.d0)), Intent(inout)    :: x(:)
  Real(Kind(1.d0)), Intent(in)       :: eps
  integer, intent(out)               :: info
!!$   Local data
  Real(Kind(1.d0)), allocatable, target   :: aux(:),wwrk(:,:)
  Real(Kind(1.d0)), allocatable   :: p(:), f(:)
  info = 0
  Call foo_pwrk(pr,p,f,cd,info,work=aux)  ! This worked if bar_pwrk was called!
  return
End Subroutine foo_sub

! { dg-final { cleanup-modules "foo_base_mod foo_pr_mod bar_pr_mod bar_prt" } }

