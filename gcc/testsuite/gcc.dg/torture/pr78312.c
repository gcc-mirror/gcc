/* { dg-do run } */

typedef unsigned short u16;

static u16 a;

u16 __attribute__ ((noinline, noclone))
foo (int p1)
{
  a = -(p1 > 0);
  a *= 0 != a;
  a *= (unsigned)a;
  return a;
}

int
main ()
{
  u16 x = foo (1);
  if (x != 1)
    __builtin_abort();
  return 0;
}
