/* PR tree-optimization/66949 */
/* { dg-do compile } */

char a;
int b, c, d;
extern int fn2 (void);

short
fn1 (short p1, short p2)
{
  return p2 == 0 ? p1 : p1 / p2;
}

int
main (void)
{
  char e = 1;
  int f = 7;
  c = a >> f;
  b = fn1 (c, 0 < d <= e && fn2 ());

  return 0;
}
