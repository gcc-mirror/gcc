/* { dg-do compile } */
/* { dg-options "-Os" } */

int a;

int
fn1 (int p)
{
  return -p;
}

void
fn2 ()
{
  fn1 (-(unsigned int) a);
}
