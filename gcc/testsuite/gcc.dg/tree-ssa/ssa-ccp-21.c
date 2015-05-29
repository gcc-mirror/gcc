/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

struct A {
    struct B {
	int i;
    } b;
} a;

int foo (void)
{
  struct B *p = &a.b;
  struct A *q = (struct A *) p;
  return q->b.i;
}

int bar (void)
{
  struct A *p = &a;
  struct B *q = (struct B *) p;
  return q->i;
}

/* The first access is through struct A, so a.b.i is fine,
   the second access needs to preserve the original access type struct B.  */

/* { dg-final { scan-tree-dump-times "a.b.i" 1 "ccp1" } } */
/* { dg-final { scan-tree-dump-times "MEM\\\[\\\(struct B \\\*\\\)&a\\\].i" 1 "ccp1" } } */
