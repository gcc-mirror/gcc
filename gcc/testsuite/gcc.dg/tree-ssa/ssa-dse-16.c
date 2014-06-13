/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1-details" } */

struct X { struct A { int a[2]; } b[10]; };
void foo (struct X *x, int i)
{
  struct A a;
 /* Confuse SRA here with using a variable index, otherwise it will mess
    with the IL too much.  */
  a.a[i] = 3;
  a.a[1] = 0;
  /* The following store is dead.  */
  x->b[i].a[0] = 1;
  x->b[i] = a;
}

/* { dg-final { scan-tree-dump "Deleted dead store" "dse1" } } */
/* { dg-final { cleanup-tree-dump "dse1" } } */
