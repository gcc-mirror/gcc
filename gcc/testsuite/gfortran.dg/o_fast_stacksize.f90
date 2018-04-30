! { dg-do compile }
! { dg-options "-Ofast -fmax-stack-var-size=100 -fdump-tree-original" }
MODULE foo
CONTAINS
  SUBROUTINE mysum(a)
    INTEGER :: a(:)
    WRITE(6,*) SUM(a)
  END SUBROUTINE
END MODULE foo

USE foo
INTEGER, ALLOCATABLE :: a(:)
INTEGER, PARAMETER :: N=2**26 ! 256Mb array
ALLOCATE(a(N)) ; a=1
CALL mysum(a*a)
END
! { dg-final { scan-tree-dump-times "__builtin_malloc" 2 "original" } }
