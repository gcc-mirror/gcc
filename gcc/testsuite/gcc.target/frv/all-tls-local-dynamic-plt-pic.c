/* { dg-options "-ftls-model=local-dynamic -minline-plt -fpic -mfdpic" } */
/* { dg-do compile } */
static __thread int x;
extern void bar ();
int *y;

void foo (void)
{
  bar ();
  y = &x;
}
/* { dg-final { scan-assembler "lddi.*gottlsdesc12" } } */
/* { dg-final { scan-assembler "calll.*#gettlsoff\\(0\\)" } } */
