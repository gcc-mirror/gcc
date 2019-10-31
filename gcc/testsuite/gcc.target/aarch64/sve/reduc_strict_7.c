/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details" } */

/* Strict FP reductions shouldn't be used for the outer loop, only the
   inner loop.  */

float
double_reduc (float (*i)[16])
{
  float l = 0;

#pragma GCC unroll 0
  for (int a = 0; a < 8; a++)
    for (int b = 0; b < 100; b++)
      l += i[b][a];
  return l;
}

/* { dg-final { scan-assembler-times {\tfadda\ts[0-9]+, p[0-7], s[0-9]+, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-tree-dump "Detected double reduction" "vect" } } */
/* { dg-final { scan-tree-dump-not "OUTER LOOP VECTORIZED" "vect" } } */
