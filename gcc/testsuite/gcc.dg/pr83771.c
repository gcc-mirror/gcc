/* PR rtl-optimization/83771 */
/* { dg-do compile } */
/* { dg-options "-O3 -fmodulo-sched -fno-ssa-phiopt" } */

long int a;
int b;
int foo (int);

void
bar (void)
{
  int c;
  do
    {
      c = a / (!!b == 1);
      c = !!c + 1;
    }
  while (foo (c) < 1);
}
