! { dg-do compile }
! { dg-options "-O3 -funconstrained-commons -fdump-tree-dom2-details" }

! Test for PR69368: a single-element array in a common block, which will be
! overridden with a larger size at link time (contrary to language spec).
! Dominator opts considers accesses to differently-computed elements of X as
! equivalent, unless -funconstrained-commons is passed in.
      SUBROUTINE FOO
      IMPLICIT DOUBLE PRECISION (X)
      INTEGER J
      COMMON /MYCOMMON / X(1)
      DO 10 J=1,1024
         X(J+1)=X(J+7)
  10  CONTINUE
      RETURN
      END
! { dg-final { scan-tree-dump-not "FIND" "dom2" } }
! We should retain both a read and write of mycommon.x.
! { dg-final { scan-tree-dump-times "  _\[0-9\]+ = mycommon\\.x\\\[_\[0-9\]+\\\];" 1 "dom2" } }
! { dg-final { scan-tree-dump-times "  mycommon\\.x\\\[j?_\[0-9\]+\\\] = _\[0-9\]+;" 1 "dom2" } }
