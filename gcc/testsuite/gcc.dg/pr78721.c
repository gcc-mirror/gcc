/* { dg-do run } */
/* { dg-options "-O2" } */

int a, b, c;

int fn1 (char e, char f)
{
  return !f || (e && f == 1);
}

void fn2 (char e)
{
  while (b)
    e = 0;
  a = 128;
  c = fn1 (e, a == e);
}

int main ()
{
  fn2 (0);
  return 0;
}
