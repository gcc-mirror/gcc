/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1-details" } */

struct X { int x; };
struct X x;

extern struct X foo (void);
void bar()
{
  x = foo();
  x = (struct X){};
}

extern struct X __attribute__((const)) foo2 (int);
void bar2()
{
  x = foo2 (1);
  x = foo2 (2);
}

/* { dg-final { scan-tree-dump-times "Deleted dead store in call LHS: x = foo " 1 "dse1" } } */
/* { dg-final { scan-tree-dump-times "Deleted dead store: x = foo2 " 1 "dse1" } } */
