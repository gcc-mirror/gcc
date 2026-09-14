/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */

void round_values(float* values)
{
  const float big = 0x1p24f;
  for (int i = 0; i < 8; ++i)
    values[i] = __builtin_assoc_barrier(values[i] + big) - big;
}

/* { dg-final { scan-tree-dump "\\\(\\\(vect__\[0-9._\]*\\\)\\\)" "vect" { target vect_float } } } */
