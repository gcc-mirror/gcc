! Check that __float128 can be used where it's supported
!
! { dg-do compile { target ia64-*-* i?86-*-* x86_64-*-* } }
! { dg-options "-fdump-tree-original" }
! { dg-final { scan-tree-dump "sqrtq" "original" } }
! { dg-final { scan-tree-dump "cabsq" "original" } }
! { dg-final { scan-tree-dump "cosl" "original" } }
! { dg-final { cleanup-tree-dump "original" } }
!
  real(kind=16) :: x1, x2
  complex(kind=16) :: z1, z2

  real(kind=10) :: y

  read (*,*) x1
  x2 = sqrt(x1)                         ! sqrtq
  z1 = x1 + (0._16 , 1.0_16)
  z2 = z1 / (1._16, 2._16)

  x1 = abs(z2)                          ! cabsq


  y = 2
  y = cos(y)                            ! cosl

  print *, x1, x2, z1, z2, y

end
