! { dg-do compile }
! { dg-options "-O2 -ftree-loop-linear -fdump-tree-ltrans-all" }
! { dg-options "-O2 -ftree-loop-linear -fdump-tree-ltrans-all -march=i486" { target { i?86-*-* && ilp32 } } }

Program FOO
  IMPLICIT INTEGER	(I-N)
  IMPLICIT REAL*8	(A-H, O-Z)
  PARAMETER (N1=1335, N2=1335)
  COMMON U(N1,N2), V(N1,N2), P(N1,N2)

  PC = 0.0D0
  UC = 0.0D0
  VC = 0.0D0

  do I = 1, M
     do J = 1, M
        PC = PC + abs(P(I,J))
        UC = UC + abs(U(I,J))
        VC = VC + abs(V(I,J))
     end do
     U(I,I) = U(I,I) * ( mod (I, 100) /100.)
  end do

  write(6,366) PC, UC, VC
366  format(/, ' PC = ',E12.4,/,' UC = ',E12.4,/,' VC = ',E12.4,/)

end Program FOO

! Please do not XFAIL.
! { dg-final { scan-tree-dump-times "transformed loop" 1 "ltrans"} }
! { dg-final { cleanup-tree-dump "ltrans" } }
