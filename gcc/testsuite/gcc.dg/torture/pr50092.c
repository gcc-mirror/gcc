/* PR target/50092 */
/* { dg-do compile { target lp64 } } */

volatile int v;

void bar (long double);
void baz (_Complex long double *);

void
foo (void)
{
  _Complex long double w[100000000];
  bar ((long double) v / 2147483648.0);
  baz (w);
}
