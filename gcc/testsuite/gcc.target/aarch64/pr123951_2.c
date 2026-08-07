/* PR tree-optimization/123951.  Like pr123951_1.c, but for generic vector
   shuffles, including ones that only become lane inserts after being
   re-encoded to a wider element mode.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned long long v2di __attribute__((vector_size (16)));
typedef unsigned int v4si __attribute__((vector_size (16)));

v2di
shuffle_03 (v2di a, v2di b)
{
  return __builtin_shuffle (a, b, (v2di) { 0, 3 });
}

v2di
shuffle_21 (v2di a, v2di b)
{
  return __builtin_shuffle (a, b, (v2di) { 2, 1 });
}

v4si
shuffle_0167 (v4si a, v4si b)
{
  return __builtin_shuffle (a, b, (v4si) { 0, 1, 6, 7 });
}

v4si
shuffle_4523 (v4si a, v4si b)
{
  return __builtin_shuffle (a, b, (v4si) { 4, 5, 2, 3 });
}

/* { dg-final { scan-assembler-times "\\tins\\t" 4 } } */
/* { dg-final { scan-assembler-not "\\tmov\\t" } } */
