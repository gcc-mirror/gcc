/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi -fdump-tree-optimized" } */
/* { dg-additional-options "-msse2" { target ia32 } } */

/* A chain of BIT_INSERT_EXPRs that writes every element of a vector, or
   that starts from a constant or a CONSTRUCTOR, is turned into a single
   CONSTRUCTOR.  */

typedef float v4sf __attribute__((vector_size(16)));
typedef int v4si __attribute__((vector_size(16)));

v4sf
splat_loop (float c)
{
  v4sf v = { 0 };
#pragma GCC unroll 4
  for (int i = 0; i < 4; i++)
    v[i] = c;
  return v;
}

v4sf
splat_insert (float c)
{
  v4sf v = { 0 };
  v[0] = c; v[1] = c; v[2] = c; v[3] = c;
  return v;
}

v4si
all_lanes (int a, int b, int c, int d)
{
  v4si v;
  v[3] = d; v[1] = b; v[0] = a; v[2] = c;
  return v;
}

v4si
overwrite (int a, int b, int c, int d)
{
  v4si v;
  v[0] = d; v[1] = b; v[2] = c; v[3] = d; v[0] = a;
  return v;
}

v4si
partial_const (int a, int b)
{
  v4si v = { 1, 2, 3, 4 };
  v[1] = a; v[3] = b;
  return v;
}

v4si
partial_ctor (int a, int b, int c)
{
  v4si v = { a, a, a, a };
  v[2] = b; v[0] = c;
  return v;
}

/* { dg-final { scan-tree-dump-not "BIT_INSERT_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump-times "= {c_\[0-9\]+\\(D\\), c_\[0-9\]+\\(D\\), c_\[0-9\]+\\(D\\), c_\[0-9\]+\\(D\\)}" 2 "optimized" { target { i?86-*-* x86_64-*-* aarch64-*-* } } } } */
/* { dg-final { scan-tree-dump-times "= {a_\[0-9\]+\\(D\\), b_\[0-9\]+\\(D\\), c_\[0-9\]+\\(D\\), d_\[0-9\]+\\(D\\)}" 2 "optimized" { target { i?86-*-* x86_64-*-* aarch64-*-* } }  } } */
/* { dg-final { scan-tree-dump "= \[{\]1, a_\[0-9\]+\\(D\\), 3, b_\[0-9\]+\\(D\\)}" "optimized" { target { i?86-*-* x86_64-*-* aarch64-*-* } }  } } */
/* { dg-final { scan-tree-dump "= {c_\[0-9\]+\\(D\\), a_\[0-9\]+\\(D\\), b_\[0-9\]+\\(D\\), a_\[0-9\]+\\(D\\)}" "optimized" { target { i?86-*-* x86_64-*-* aarch64-*-* } }  } } */
