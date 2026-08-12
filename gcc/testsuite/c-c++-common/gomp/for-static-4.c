/* { dg-do compile } */
/* { dg-additional-options "-fopenmp-ompt -fopenmp-ompt=minimal -fdump-tree-ompexp" } */

/* Check that minimal OMPT callbacks are generated (same as for-static-1.c).  */

#include "for-static.h"

/* { dg-final { scan-tree-dump-times "GOMP_loop_static_worksharing \\(\[^\\)\]" 2 "ompexp" } } */
/* { dg-final { scan-tree-dump-not "GOMP_loop_static_worksharing_start" "ompexp" } } */
/* { dg-final { scan-tree-dump-not "GOMP_loop_static_worksharing_dispatch" "ompexp" } } */
/* { dg-final { scan-tree-dump-not "GOMP_loop_static_worksharing_end" "ompexp" } } */
/* { dg-final { scan-tree-dump-times "GOMP_distribute_static_worksharing \\(\[^\\)\]" 2 "ompexp" } } */
