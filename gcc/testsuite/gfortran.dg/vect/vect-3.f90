! { dg-do compile }
! { dg-require-effective-target vect_float }
! { dg-additional-options "--param vect-max-peeling-for-alignment=0" }

SUBROUTINE SAXPY(X, Y, A, N)
DIMENSION X(N), Y(N)
Y = Y + A * X
END

! { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 3 "vect" { target { vect_no_align && { ! vect_hw_misalign } } } } }
! { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" { target { {! vect_no_align} && { {! vector_alignment_reachable} && {! vect_hw_misalign} } } } } }

