/* { dg-require-effective-target vect_char_add } */
/* { dg-additional-options "-O3" } */
#define TYPE signed char
#define MIN -128
#define MAX 127
#define N_BASE (MAX - 32)
#define N_BASE_DOWN (MIN + 32)

#define C 3

#include "pr101145.inc"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
