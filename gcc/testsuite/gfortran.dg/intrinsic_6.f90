! { dg-do compile }
! { dg-options "-fimplicit-none" }
!
! PR 45748: [4.5/4.6 Regression] -fimplicit-none failures when using intrinsic MAX
!
! Contributed by Themos Tsikas <themos.tsikas@gmail.com>

SUBROUTINE BUG(WORK)
  INTRINSIC MAX
  DOUBLE PRECISION WORK(MAX(2,3))
END
