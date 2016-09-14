/* PR middle-end/45423 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple -g0 -O2 -Wno-deprecated" } */
/* atomicvar should never be referenced in between the barrier and
   following #pragma omp atomic_load.  */
/* { dg-final { scan-tree-dump-not "barrier\[^#\]*atomicvar" "gimple" } } */

#include "atomic-12.c"
