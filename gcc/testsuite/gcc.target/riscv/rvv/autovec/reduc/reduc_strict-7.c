/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -fdump-tree-vect-details" } */

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

/* { dg-final { scan-assembler-times {vle32\.v} 2 } } */
/* { dg-final { scan-assembler-times {vfredosum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-tree-dump "Detected double reduction" "vect" } } */
/* { dg-final { scan-tree-dump-not "OUTER LOOP VECTORIZED" "vect" } } */
