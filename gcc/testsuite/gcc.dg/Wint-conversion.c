/* { dg-do compile } */
/* { dg-options "-Wno-int-conversion" } */

int fn1 (int *), *fn2 (int);

int
fn1 (int *p)
{
  int i = p;
  i = p;
  fn2 (p);
  return p;
}

int *
fn2 (int i)
{
  int *p = i;
  p = i;
  fn1 (i);
  return i;
}
