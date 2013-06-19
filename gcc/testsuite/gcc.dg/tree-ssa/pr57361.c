/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1-details" } */

struct A { int x; double y; };
void f (struct A *a) {
  *a = *a;
}

/* { dg-final { scan-tree-dump "Deleted dead store" "dse1"} } */
