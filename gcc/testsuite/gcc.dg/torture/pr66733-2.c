/* { dg-do compile } */

int a, b, c, e, f;

void fn1 (int p) { }

int
fn2 (int p)
{
  return a ? p % a : 0; 
}

short
fn3 (int p)
{
  return (1 >> p) < 1 ? 1 : p;
}

int
fn4 ()
{
  int g = 0, h = 1;
  if (b)
    goto lbl;
  fn2 (0);
  if (fn3 (1))
    fn1 (e && c);
  if (h)
    {
      int i = 1;
lbl:
      if (i)
	return 0;
      for (; g < 1; g++)
	;
    }
  for (;;)
    f || g > 0;
}

int
main ()
{
  fn4 (); 
  return 0;
}
