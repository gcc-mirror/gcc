! { dg-do compile }
! { dg-options "-fcoarray=single -std=f2008" }
!
!
! CO_SUM/CO_MIN/CO_MAX
!
program test
  implicit none
  intrinsic co_max ! { dg-error "is not available in the current standard settings but new in TS 29113/TS 18508." }
  intrinsic co_min ! { dg-error "is not available in the current standard settings but new in TS 29113/TS 18508." }
  intrinsic co_sum ! { dg-error "is not available in the current standard settings but new in TS 29113/TS 18508." }
end program test
