/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=neoverse-v3 -msve-vector-bits=scalable -fdump-tree-vect-details" } */

/* Account for the serial FADDA operations when comparing vector modes.
   Prefer a partial byte vector matching the float vector's lane count,
   avoiding predicate unpacks.  */
float
conditional_sum (const float *restrict a, const unsigned char *restrict c, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    if (c[i])
      sum += a[i];
  return sum;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
/* { dg-final { scan-tree-dump "Choosing vector mode VNx4QI" "vect" } } */
/* { dg-final { scan-assembler {\tfadda\ts[0-9]+,} } } */
/* { dg-final { scan-assembler-not {\t[psu]unpk(lo|hi)\t} } } */
