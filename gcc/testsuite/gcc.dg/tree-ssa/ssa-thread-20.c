/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ethread-stats" } */

struct S { int base; };
void foo (struct S *p)
{
  if (p)
    {
      int *q = &p->base;
      if (q)
        __builtin_puts ("x");
    }
}

/* { dg-final { scan-tree-dump "Jumps threaded: 1" "ethread" } } */
