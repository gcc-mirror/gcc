/* { dg-do compile { target { i?86-*-* x86_64-*-*  } } } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

void test (int *b, int *e, int stride)
  {
    for (int *p = b; p != e; p += stride)
      *p = 1;
  }

/* { dg-final { scan-tree-dump-times "PHI <p" 1 "ivopts"} } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */
