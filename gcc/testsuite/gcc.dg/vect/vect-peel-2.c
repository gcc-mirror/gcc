/* { dg-require-effective-target vect_int } */
/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */

#include "vect-peel-2-src.c"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { target { { vect_element_align } && { vect_aligned_arrays } } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { target { { vect_element_align } && { vect_aligned_arrays } } } } } */
