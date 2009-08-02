! { dg-do compile }
! { dg-options "-std=legacy" }
!
! PR 24643
! substring references on implicitly typed CHARACTER variables didn't work
        PROGRAM P
        IMPLICIT CHARACTER*8 (Y)
        YLOCAL='A'
        YBTABLE=YLOCAL(1:2)
        END
