/* { dg-options "-ftls-model=local-dynamic -minline-plt -fPIC -mfdpic" } */
/* { dg-do compile } */
static __thread int x;
extern void bar ();
int *y;

void foo (void)
{
  bar ();
  y = &x;
}
/* { dg-final { scan-assembler "ldd.*tlsdesc\\(0\\)@" } } */
