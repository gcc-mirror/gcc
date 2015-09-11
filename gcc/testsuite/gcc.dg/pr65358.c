/* { dg-do run } */
/* { dg-options "-O2" } */

struct pack
{
  int fine;
  int victim;
  int killer;
};

int __attribute__ ((__noinline__, __noclone__))
bar (int a, int b, struct pack p)
{
  if (a != 20 || b != 30)
    __builtin_abort ();
  if (p.fine != 40 || p.victim != 50 || p.killer != 60)
    __builtin_abort ();
  return 0;
}

int __attribute__ ((__noinline__, __noclone__))
foo (int arg1, int arg2, int arg3, struct pack p)
{
  return bar (arg2, arg3, p);
}

int main (void)
{
  struct pack p = { 40, 50, 60 };

  (void) foo (10, 20, 30, p);
  return 0;
}
