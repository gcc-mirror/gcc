/* PR target/94440 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-mfpmath=sse,387 -msse2 -Ofast -fexcess-precision=standard -fno-graphite --param=scev-max-expr-size=0" } */

int b;
double c, d;

void
foo (void)
{
  int i;
  for (i = 0; i < b; i++)
    c += i;
}

int __attribute__((optimize(1)))
main ()
{
  double a[9];
  int i;
  for (i = 0; i < 9; i++)
    d += a[i];
}
