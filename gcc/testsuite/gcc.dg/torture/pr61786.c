/* { dg-do run } */

int a, *c = &a, d; 
char b = 1;

void
fn1 ()
{
  d = 1;
lbl:
  if (b == d)
    {
      d = *c;
      if (b)
	goto lbl;
    }
}

int
fn2 ()
{
  fn1 ();
  return 0;
}

int
main ()
{
  fn2 ();
  return 0;
}
