! { dg-do compile }
! { dg-options "-fcoarray=single -std=f2008" }
!
!
! CO_REDUCE/CO_BROADCAST
!
program test
  implicit none
  intrinsic co_reduce ! { dg-error "is not available in the current standard settings but new in TS 29113/TS 18508." }
  intrinsic co_broadcast ! { dg-error "is not available in the current standard settings but new in TS 29113/TS 18508." }
end program test
