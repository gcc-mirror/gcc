/* Permute chains reading only a and b, including one built up from an
   uninitialized vector, fold to a single VEC_PERM_EXPR.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi -fdump-tree-optimized" } */
/* { dg-additional-options "-msse2" { target i?86-*-* x86_64-*-* } } */

typedef unsigned int v4su __attribute__((vector_size(16)));

v4su
f1 (v4su a, v4su b)
{
  v4su d;
  d[0] = a[2];
  d[1] = a[3];
  d[2] = b[2];
  d[3] = b[3];
  return d;
}

v4su
f2 (v4su a, v4su b)
{
  v4su d = a;
  d[0] = a[2];
  d[1] = a[3];
  d[2] = b[2];
  d[3] = b[3];
  return d;
}

/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR <a_\[0-9\]+\\(D\\), b_\[0-9\]+\\(D\\), \{ 2, 3, 6, 7 \}>" 2 "optimized" { target { aarch64*-*-* i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 2 "optimized" { target { aarch64*-*-* i?86-*-* x86_64-*-* } } } } */
