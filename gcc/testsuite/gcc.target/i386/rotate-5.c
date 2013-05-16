/* { dg-do compile } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -mavx -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

unsigned int a[1024] __attribute__((aligned (32)));

__attribute__((noinline, noclone)) void
foo (void)
{
  int i, j = 3;
  for (i = 0; i < 1024; i++)
    a[i] = (a[i] << j) | (a[i] >> ((-j) & 31));
}
