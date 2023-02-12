/* PR target/108308 */
/* { dg-do run { target int32 } } */
/* { dg-options "-Os -fno-tree-ccp" } */

int a = 1, *d = &a, f = 2766708631, h;
unsigned b = -1, c, e, g;

static void
foo (int j)
{
  if (a)
    {
      c = ~c;
      while (e)
	j = 0;
      goto k;
    }
l:
  h = 1;
k:
  *d = (!j) | 80;
  int m = ~(~(-1 / b) | (a ^ 1)), n = ~(~g / (11 >> m)), o = -1 / n;
  if (f)
    {
      b = 9518150474215344 ^ ~f;
      f = 0;
      if (c)
	goto l;
      if (o)
	goto k;
    }
}

int
main ()
{
  foo (1);
  return 0;
}
