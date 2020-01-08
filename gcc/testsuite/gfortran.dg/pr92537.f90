! { dg-do compile }
! { dg-options "-O2 -ftree-vectorize -fno-inline" }
! { dg-additional-options "-march=skylake" { target x86_64-*-* i?86-*-* } }
MODULE pr93527
  implicit none
  integer, parameter :: wp = kind (1.d0)
  interface p_min
     module procedure p_min_wp
  end interface
contains
  subroutine foo (pr)
    real(wp), pointer     :: pr(:)
    integer  ::  nzd
    real(wp) ::  pmin
    real(wp) ::  pmin_diag
    integer  ::  i
    nzd  = 15
    allocate (pr(nzd))
    pmin_diag = 4000._wp
    pmin = p_min(pmin_diag)
    pmin = min (pmin,pmin_diag)
    pr(1) = log(pmin)
    do i=1,nzd-1
       pr(i+1) = log(pmin) + i
    end do
  end subroutine foo
  function p_min_wp (x) result (p_min)
    real(wp), intent(in) :: x
    real(wp)             :: p_min
    p_min = x
  end function p_min_wp
end module pr93527
