/* { dg-require-effective-target vect_long_long } */
/* { dg-additional-options "-O3" } */
#define TYPE int *
#define MIN ((TYPE)0)
#define MAX ((TYPE)((long long)-1))
#define N_BASE (MIN - 32)
#define N_BASE_DOWN (MIN + 32)

#define C 1

#include "pr101145.inc"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
