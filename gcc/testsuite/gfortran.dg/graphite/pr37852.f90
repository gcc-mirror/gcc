! { dg-options "-O2 " }

PROGRAM TEST_FPU
CHARACTER (LEN=36) :: invert_id(1) = &
                      (/ 'Test1 - Gauss 2000 (101x101) inverts'/)
END PROGRAM TEST_FPU

SUBROUTINE Gauss (a,n)
INTEGER, PARAMETER :: RK8 = SELECTED_REAL_KIND(15, 300)
REAL(RK8) :: a(n,n)
INTEGER ::  ipvt(n)
a(:,ipvt) = b
END SUBROUTINE Gauss
