/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */
#define TYPE unsigned char
#define MIN 0
#define MAX 255
#define N_BASE (MAX - 32 + 1)
#define N_BASE_DOWN (MIN + 32)

#define C 2

#include "pr101145.inc"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
