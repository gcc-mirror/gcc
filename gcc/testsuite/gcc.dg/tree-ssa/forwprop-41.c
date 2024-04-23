/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -Wno-psabi -w" } */

#define vector __attribute__((__vector_size__(16) ))

vector int g(vector int a, int c)
{
  int b = a[2];
  a[2] = b;
  a[1] = c;
  return a;
}

/* { dg-final { scan-tree-dump-times "BIT_INSERT_EXPR" 1 "optimized" { xfail s390_mvx } } } Xfail PR114802 */
/* { dg-final { scan-tree-dump-times "BIT_FIELD_REF" 0 "optimized" { xfail s390_mvx } } } Xfail PR114802 */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 0 "optimized" } } */
