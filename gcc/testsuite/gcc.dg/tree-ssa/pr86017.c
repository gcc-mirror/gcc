/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-store-merging" } */

void f (void*);

void g (void)
{
  char a[8];
  __builtin_memset (a, 0, 8);

  f (a);
}

void h (void)
{
  char a[8];
  __builtin_memset (a, 0, 1);
  __builtin_memset (a + 1, 0, 1);
  __builtin_memset (a + 2, 0, 1);
  __builtin_memset (a + 3, 0, 1);
  __builtin_memset (a + 4, 0, 1);
  __builtin_memset (a + 5, 0, 1);
  __builtin_memset (a + 6, 0, 1);
  __builtin_memset (a + 7, 0, 1);

  f (a);
}

/* { dg-final { scan-tree-dump "Merged into 1 stores" "store-merging" } } */
