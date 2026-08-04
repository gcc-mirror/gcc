/* { dg-do compile } */
/* { dg-options "-O3 -mautovec-preference=sve-only -msve-vector-bits=scalable -fdump-tree-vect-details" } */

int __attribute__ ((noipa))
c (int *restrict x, int *restrict y, int n)
{
  unsigned d = 5;
  for (; __builtin_expect (d < n, 1); ++d)
    {
      if (x[d] != y[d])
	return 0;
    }
  return 1;
}

/* { dg-final { scan-tree-dump "Both peeling and versioning will be applied" "vect" } } */
/* { dg-final { scan-tree-dump "misalignment for fully-masked loop" "vect" } } */
/* { dg-final { scan-assembler {\tsub\tw[0-9]+, w[0-9]+, #6} } } */
/* { dg-final { scan-assembler-times {\twhilelo\t} 3 } } */
/* { dg-final { scan-assembler-times {\tptest\t} 0 } } */
/* { dg-final { scan-assembler-times {\tld1w\t} 2 } } */
