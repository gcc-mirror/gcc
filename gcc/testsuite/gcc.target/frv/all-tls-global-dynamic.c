/* { dg-options "-ftls-model=global-dynamic -fpic -mfdpic -mno-inline-plt" } */
/* { dg-do compile } */
extern __thread int x;
extern void bar ();
int *y;

void foo (void)
{
  bar ();
  y = &x;
}
/* { dg-final { scan-assembler "call #gettlsoff.x." } } */
