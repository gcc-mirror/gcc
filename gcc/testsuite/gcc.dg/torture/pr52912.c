/* { dg-do compile } */

int a, b, c;
static int
fn1 (int p1)
{
lbl_549:
      if (p1)
	        goto lbl_549;
          return 0;
}

void
fn2 ()
{
      b = (c && a) > fn1 (c) >= c;
}
