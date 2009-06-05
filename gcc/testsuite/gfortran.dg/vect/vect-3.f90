! { dg-do compile }
! { dg-require-effective-target vect_float }

SUBROUTINE SAXPY(X, Y, A, N)
DIMENSION X(N), Y(N)
Y = Y + A * X
END

! { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 3 "vect" { target vect_no_align } } }
! { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { target vect_no_align } } } 
! { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" { target { {! vect_no_align} && { {! vector_alignment_reachable} && {! vect_hw_misalign} } } } } }
! { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { target { {! vect_no_align} && { {! vector_alignment_reachable} && {! vect_hw_misalign} } } } } }
! { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 3 "vect" {target vect_hw_misalign} } }
! { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail { { vect_no_align || vect_hw_misalign } || {! vector_alignment_reachable}} } } }
! { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { xfail { { vect_no_align || vect_hw_misalign } || { ! vector_alignment_reachable} } } } }

! { dg-final { cleanup-tree-dump "vect" } }
