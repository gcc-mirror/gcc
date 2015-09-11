! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! Test fix for PR55789
!
! Contributed by Joost VandVandole  <Joost.VandeVondele@mat.ethz.ch>
!
MODULE M1
CONTAINS
  SUBROUTINE cp_1d_i4_sort(arr)
      INTEGER(kind=4), DIMENSION(:), &
        INTENT(inout)                          :: arr
      arr = (/ (i, i = 1, SIZE(arr)) /)
  END SUBROUTINE
END MODULE M1

PROGRAM TEST
  USE M1
  INTEGER :: arr(1)
  INTERFACE
    SUBROUTINE mtrace() BIND(C,name="mtrace")
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE muntrace() BIND(C,name="muntrace")
    END SUBROUTINE
  END INTERFACE
  CALL mtrace()
  CALL cp_1d_i4_sort(arr)
  CALL muntrace()
END

! { dg-final { scan-tree-dump-times "realloc" 0 "original" } }
