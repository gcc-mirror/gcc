/* PR target/94440 */
/* { dg-do compile } */
/* { dg-options "-O0 -msse2 -ffast-math" } */

double a;
int b;
long double c;

void
foo (void)
{
  float d = (double)(long double)b;
}

__attribute__((optimize("O0"))) void
bar (void)
{
  a = c;
}
