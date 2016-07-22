/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -w -Wno-psabi" } */

#define foobar(n) \
    typedef int v##n##si __attribute__ ((vector_size (4 * n))); \
\
int \
foo##n(int x, v##n##si v) \
{ \
  v[0] ^= v[1]; \
  return ((v##n##si)v)[x]; \
} \
\
int \
bar##n(int x, v##n##si v) \
{ \
  v[0] ^= v[1]; \
  return v[x]; \
}

foobar(2)
foobar(4)
foobar(8)
foobar(16)
foobar(32)
foobar(64)

/* Verify we don't have any vector temporaries in the IL.  */
/* { dg-final { scan-tree-dump-not "vector" "optimized" } } */
