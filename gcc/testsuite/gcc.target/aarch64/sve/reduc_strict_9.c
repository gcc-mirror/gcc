/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details" } */

float
double_reduc (float *i, float *j)
{
  float k = 0, l = 0;

  for (int a = 0; a < 8; a++)
    for (int b = 0; b < 100; b++)
      {
        k += i[b];
        l += j[b];
      }
  return l * k;
}

/* { dg-final { scan-assembler-times {\tld1w\t} 2 } } */
/* { dg-final { scan-assembler-times {\tfadda\ts[0-9]+, p[0-7], s[0-9]+, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-tree-dump "Detected double reduction" "vect" } } */
/* { dg-final { scan-tree-dump-not "OUTER LOOP VECTORIZED" "vect" } } */
