/* { dg-options "-ftls-model=local-dynamic -fpic -mfdpic" } */
/* { dg-do compile } */
static __thread int x;
extern void bar ();
int *y;

void foo (void)
{
  bar ();
  y = &x;
}
/* { dg-final { scan-assembler "gettlsoff\\(0\\)" } } */
/* { dg-final { scan-assembler "tlsmoff12" } } */
