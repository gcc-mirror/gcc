/* PR tree-optimization/82374 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-vect-details" } */
/* { dg-additional-options "-mavx -mno-avx2" { target i?86-*-* x86_64-*-* } } */
/* { dg-additional-options "-mvsx" { target powerpc_vsx_ok } } */

#define SIZE (1024 * 1024)

float a[SIZE];
float b[SIZE];
float c[SIZE];
float d[SIZE];

__attribute__((optimize ("O2", "tree-vectorize"))) void
foo (void)
{
  int i;
#pragma omp parallel for
  for (i = 0; i < SIZE; i++)
    c[i] = a[i] + b[i];
}

__attribute__((optimize ("O2", "tree-vectorize"))) void
bar (void)
{
  int i;
  for (i = 0; i < SIZE; i++)
    d[i] = a[i] + b[i];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target { { i?86-*-* x86_64-*-* } || { powerpc_vsx_ok } } } } } */
