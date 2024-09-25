/* { dg-do compile } */
/* { dg-require-effective-target le } */
/* { dg-additional-options "-O1" } */

typedef unsigned int v4si __attribute__ ((vector_size (16)));

v4si f1 (v4si a)
{
  v4si zeros = {0,0,0,0};
  return __builtin_shufflevector (zeros, a, 0, 5, 1, 6);
}

v4si f2 (v4si a)
{
  v4si zeros = {0,0,0,0};
  return __builtin_shufflevector (a, zeros, 0, 5, 1, 6);
}

/* { dg-final { scan-assembler-times {tbl\tv[0-9]+.16b, \{v[0-9]+.16b\}, v[0-9]+.16b} 2 } } */
/* { dg-final { scan-assembler-times {(\.byte\s+-1\n\s+){4}(\.byte\s+[4-7]+\n\s+){4}(\.byte\s+-1\n\s+){4}(\.byte\s+(8|9|10|11)+\n?\s*){4}} 1 } } */
