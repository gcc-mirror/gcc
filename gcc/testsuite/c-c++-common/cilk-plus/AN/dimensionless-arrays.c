/* { dg-options "-fcilkplus" } */

extern int a[];
extern int *b;

void foo()
{
  a[:] = 5;	// { dg-error "start-index and length fields necessary for using array notation" }
  b[:] = 5;    // { dg-error "start-index and length fields necessary for using" }
}
