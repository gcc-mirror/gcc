/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */
/* { dg-add-options bind_pic_locally } */

#include "vect-peel-3-src.c"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect"  { xfail { { ! vect_unaligned_possible } || vect_sizes_32B_16B } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail { { ! vect_unaligned_possible } || vect_sizes_32B_16B } } } } */
