/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -mno-avx512f -fdump-tree-vect-details" } */

/* An exact division by a power of two, such as a pointer difference, is
   vectorized as an arithmetic shift without the rounding adjustment of
   negative dividends needed for truncating division.  */

void
f (long *__restrict out, int **__restrict p, int **__restrict q)
{
  for (int i = 0; i < 1024; i++)
    out[i] = p[i] - q[i];
}

/* { dg-final { scan-tree-dump "divmod pattern recognized" "vect" } } */
/* { dg-final { scan-assembler-not "vpcmp" } } */
