/* { dg-lto-do run } */
/* { dg-lto-options { { -flto } } } */
/* { dg-extra-ld-options "-O" } */

int a, *c = &a, d;
char b = 1;

void
fn1 (void)
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
fn2 (void)
{
  fn1 ();
  return 0;
}

