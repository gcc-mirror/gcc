/* { dg-options "-O2" } */
/* PR target/100942 */

void foo(void);
int f1(int a, int b)
{
  int c = a == 0 || b == 0;
  if (c) foo();
  return c;
}

/* We should get one cmp followed by ccmp and one cset. */
/* { dg-final { scan-assembler "\tccmp\t" } } */
/* { dg-final { scan-assembler-times "\tcset\t" 1 } } */
/* { dg-final { scan-assembler-times "\tcmp\t" 1 } } */
/* And not get 2 cmps and 2 (or more cset) and orr and a cbnz. */
/* { dg-final { scan-assembler-not "\torr\t" } } */
/* { dg-final { scan-assembler-not "\tcbnz\t" } } */
/* { dg-final { scan-assembler-not "\tcbz\t" } } */

