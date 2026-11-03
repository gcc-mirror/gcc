/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O3 -fvect-cost-model=unlimited -fdump-tree-vect-details" } */
/* { dg-add-options arm_neon } */

void
negate_array (unsigned long long *__restrict output,
              const unsigned long long *__restrict input, unsigned int count)
{
  for (unsigned int i = 0; i < count; ++i)
    output[i] = -input[i];
}

/* { dg-final { scan-assembler {vsub\.i64} } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
