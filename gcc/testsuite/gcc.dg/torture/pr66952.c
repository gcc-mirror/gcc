/* { dg-do run } */

int a = 128, b;

static int
fn1 (signed char p1, int p2)
{
  return p1 < 0 || p1 > 1 >> p2 ? 0 : p1 << 1;
}

static int
fn2 ()
{
  signed char c = a;
  b = fn1 (c, 1);
  if ((128 | c) < 0 ? 1 : 0)
    return 1;
  return 0;
}

int
main ()
{
  if (fn2 () != 1)
    __builtin_abort ();

  return 0;
}
