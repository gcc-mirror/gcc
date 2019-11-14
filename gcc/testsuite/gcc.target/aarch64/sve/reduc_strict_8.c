/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details" } */

float
double_reduc (float *i)
{
  float l = 0;

  for (int a = 0; a < 8; a++)
    for (int b = 0; b < 16; b++)
      {
        l += i[b * 4];
        l += i[b * 4 + 1];
        l += i[b * 4 + 2];
        l += i[b * 4 + 3];
      }
  return l;
}

/* { dg-final { scan-assembler-times {\tfadda\ts[0-9]+, p[0-7], s[0-9]+, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-tree-dump "Detected double reduction" "vect" } } */
/* { dg-final { scan-tree-dump-not "OUTER LOOP VECTORIZED" "vect" } } */
