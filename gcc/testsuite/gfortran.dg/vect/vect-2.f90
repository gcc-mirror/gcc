! { dg-do compile }
! { dg-require-effective-target vect_float }
! { dg-additional-options "--param vect-max-peeling-for-alignment=0" }

SUBROUTINE FOO(A, B, C)
DIMENSION A(1000000), B(1000000), C(1000000)
READ*, X, Y
A = LOG(X); B = LOG(Y); C = A + B
PRINT*, C(500000)
END

! First loop (A=LOG(X)) is vectorized using peeling to align the store.
! Same for the second loop (B=LOG(Y)).
! Third loop (C = A + B) is vectorized using versioning (for targets that don't
! support unaligned loads) or using peeling to align the store (on targets that 
! support unaligned loads).

! { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" } }
! { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 3 "vect" { target { { vect_no_align && { ! vect_hw_misalign } } || { { ! vector_alignment_reachable  } && { ! vect_hw_misalign } } } } } }
