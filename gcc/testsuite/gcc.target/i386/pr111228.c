/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */
/* { dg-additional-options "-msse2" { target sse2 } } */

typedef __UINT64_TYPE__ v2di __attribute__((vector_size(16)));

v2di g;
void test (v2di *v)
{
  v2di lo = v[0];
  v2di hi = v[1];
  v2di res;
  res[1] = hi[1];
  res[0] = lo[0];
  g = res;
}

/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR <\[^>\]*, { 0, 3 }>" 1 "cddce1" { target sse2 } } } */
