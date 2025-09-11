/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */

struct A { int x; int y; };
struct B { struct A a; int k; };

int foo (struct A *a, struct B *b)
{
  *a = (struct A){};
  *b = (struct B){};
  return a->x;
}

/* { dg-final { scan-tree-dump "Skipping possible redundant definition" "fre1" } } */
/* { dg-final { scan-tree-dump "return 0;" "fre1" } } */
