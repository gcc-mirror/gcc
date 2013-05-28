/* { dg-options "-fcilkplus" } */

extern int a[];
extern int *b;

void foo()
{
  a[:] = 5;	// { dg-error "start-index and length fields necessary for using array notations in dimensionless arrays" }
  b[:] = 5;    // { dg-error "start-index and length fields necessary for using array notations in pointers" }
}
