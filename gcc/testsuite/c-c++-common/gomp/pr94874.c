/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#include <stddef.h>

size_t
vla (int array_li)
{
  float array[array_li];
  size_t size1, size2;

#pragma omp parallel default(none) shared(size1, array)
  size1 = sizeof array;

#pragma omp target defaultmap(none) map(from:size2) map(alloc:array)
  size2 = sizeof array;

  return size1 + size2;
}

/* { dg-final { scan-tree-dump "omp parallel .*shared\\(array_li\.\[0-9\]\\)" "gimple" } } */
/* C */
/* { dg-final { scan-tree-dump "omp target .*private\\(array_li\.\[0-9\]\\)" "gimple" { target { ! c++ } } } } */
/* C++ */
/* { dg-final { scan-tree-dump "omp target .*firstprivate\\(array_li\.\[0-9\]\\)" "gimple" { target { c++ } } } } */
