! { dg-do compile }
! { dg-require-effective-target vect_float }

SUBROUTINE FOO(A, B, C)
DIMENSION A(1000000), B(1000000), C(1000000)
READ*, X, Y
A = LOG(X); B = LOG(Y); C = A + B
PRINT*, C(500000)
END

! { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" { xfail vect_no_align } } }
! { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 3 "vect" { xfail vect_no_align } } }
! { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { xfail vect_no_align } } }
! { dg-final { cleanup-tree-dump "vect" } }
