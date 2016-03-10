/* PR70128 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing -fdump-tree-optimized" } */

void foo (int b)
{
  extern void bar (void);
  extern void baz (void);
  void *p;
  if (b)
    p = bar;
  else
    p = baz;
  *(char *)p = 1;
}

/* We should keep the store to the function locations.  */
/* { dg-final { scan-tree-dump " = 1;" "optimized" } } */
