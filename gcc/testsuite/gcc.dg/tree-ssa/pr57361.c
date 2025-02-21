/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1-details -fno-tree-forwprop" } */

struct A { int x; double y; };
void f (struct A *a) {
  *a = *a;
}

/* { dg-final { scan-tree-dump "Deleted dead store" "dse1"} } */
