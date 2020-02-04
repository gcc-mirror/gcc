/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

struct A { int a[1]; };

void f (struct A *p)
{
  void *q = p->a;
  if (p != q)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "abort" "forwprop1" } } */
