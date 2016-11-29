/* { dg-require-effective-target int32plus } */

/* PR tree-optimization/78170.
   Check that sign-extended store to a bitfield
   doesn't overwrite other fields.  */

int a, b, d;

struct S0
{
  int f0;
  int f1;
  int f2;
  int f3;
  int f4;
  int f5:15;
  int f6:17;
  int f7:2;
  int f8:30;
} c;

void fn1 ()
{
  d = b = 1;
  for (; b; b = a)
    {
      struct S0 e = { 0, 0, 0, 0, 0, 0, 1, 0, 1 };
      c = e;
      c.f6 = -1;
    }
}

int main ()
{
  fn1 ();
  if (c.f7 != 0)
    __builtin_abort ();
  return 0;
}
