/* PR tree-optimization/66949 */
/* { dg-do compile } */

int a, *b = &a, c;

unsigned short
fn1 (unsigned short p1, unsigned int p2)
{
  return p2 > 1 || p1 >> p2 ? p1 : p1 << p2;
}

void
fn2 ()
{
  int *d = &a;
  for (a = 0; a < -1; a = 1)
    ;
  if (a < 0)
    c = 0;
  *b = fn1 (*d || c, *d);
}

int
main ()
{
  fn2 ();
  return 0;
}
