/* { dg-do compile } */

int a, b;
int
fn1 (int p1)
{
  return p1 < 0 ? p1 : a;
}

void
fn2 ()
{
lbl_100:
  b = 1;
  for (; b != 21; b = fn1 (b))
    ;
  goto lbl_100;
}
