/* { dg-options "-ftls-model=local-exec -mfdpic -mTLS" } */
/* { dg-do compile } */
static __thread int x;
extern void bar ();
int *y;

void foo (void)
{
  bar ();
  y = &x;
}
/* { dg-final { scan-assembler "sethi.*tlsmoffhi\\(x\\)," } } */
