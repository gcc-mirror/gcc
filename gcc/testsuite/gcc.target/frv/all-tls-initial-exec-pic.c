/* { dg-options "-ftls-model=initial-exec -fpic -mfdpic" } */
/* { dg-do compile } */
extern __thread int x;
extern void bar ();
int *y;

void foo (void)
{
  bar ();
  y = &x;
}
/* { dg-final { scan-assembler "ld.*#gottlsoff12" } } */
