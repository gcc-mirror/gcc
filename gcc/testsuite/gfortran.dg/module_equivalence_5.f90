! { dg-do run }
!
! Fixes PR37787 where the EQUIVALENCE between QLA1 and QLA2 wasn't recognized
! in the dependency checking because the compiler was looking in the wrong name
! space.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
module stuff
  integer, parameter :: r4_kv = 4
contains

  SUBROUTINE CF0004
!  COPYRIGHT 1999   SPACKMAN & HENDRICKSON, INC.
    REAL(R4_KV), dimension (10) :: QLA1, QLA2, QLA3, &
                                   QCA = (/(i, i= 1, 10)/)
    EQUIVALENCE (QLA1, QLA2)
    QLA1 = QCA
    QLA3 = QCA
    QLA3( 2:10:3) = QCA ( 1:5:2) + 1
    QLA1( 2:10:3) = QLA2( 1:5:2) + 1  !failed because of dependency
    if (any (qla1 .ne. qla3)) call abort
  END SUBROUTINE
end module

program try_cf004
  use stuff
  nf1 = 1
  nf2 = 2
  call cf0004
end
