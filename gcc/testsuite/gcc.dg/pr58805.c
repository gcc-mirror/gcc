/* { dg-do compile } */
/* { dg-options "-O2 -ftree-tail-merge -fdump-tree-pre" } */

/* Type that matches the 'p' constraint.  */
#define TYPE void *

static inline
void bar (TYPE *r)
{
  TYPE t;
  __asm__ ("" : "=&p" (t), "=p" (*r));
}

void
foo (int n, TYPE *x, TYPE *y)
{
  if (n == 0)
    bar (x);
  else
    bar (y);
}

/* { dg-final { scan-tree-dump-times "__asm__" 2 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
