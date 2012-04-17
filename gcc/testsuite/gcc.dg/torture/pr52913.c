/* { dg-do compile } */

int a, b, c, d, e;
void
fn1 ()
{
lbl_101:
  e = 0;
lbl_274:
  for (c = 0; c < 1; c = a)
    if (d)
      if (b)
	goto lbl_101;
      else
	break;
  d = 1;
  goto lbl_274;
}
