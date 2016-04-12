/* PR rtl-optimization/70596 */
/* { dg-do compile { target avx512f } } */
/* { dg-options "-O2 -fno-dce -fschedule-insns -fno-tree-coalesce-vars -fno-tree-dce -fno-tree-fre -fno-tree-pre -fcompare-debug -mavx512f" } */

typedef char V __attribute__((vector_size (64)));

int
foo (V u, V v)
{
  v /= u | 1;
  v[18] = 1 | v[8];
  return v[1] + v[6] + v[0] + v[1] + v[18] +  v[2] + v[7];
}
