/* { dg-options "-ftls-model=local-exec -mfdpic" } */
/* { dg-do compile } */
static __thread int x;
extern void bar ();
int *y;

void foo (void)
{
  bar ();
  y = &x;
}
/* { dg-final { scan-assembler ".*tlsmoff12" } } */
