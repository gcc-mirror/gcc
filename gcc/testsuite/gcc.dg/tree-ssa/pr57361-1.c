/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1-details" } */

struct A { int x; double y; };
void f (struct A *a) {
  *a = *a;
}

/* xfailed until figuring out the best way to handle aliasing barriers. */
/* { dg-final { scan-tree-dump "into a NOP" "forwprop1" { xfail *-*-* } } } */
