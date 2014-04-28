/* { dg-do run } */
/* { dg-options "-O1" } */

struct S
{
  int i, j;
};

struct Z
{
  struct S d, s;
};

struct S __attribute__ ((noinline, noclone))
get_s (void)
{
  struct S s;
  s.i = 5;
  s.j = 6;

  return s;
}

struct S __attribute__ ((noinline, noclone))
get_d (void)
{
  struct S d;
  d.i = 0;
  d.j = 0;

  return d;
}

int __attribute__ ((noinline, noclone))
get_c (void)
{
  return 1;
}

int __attribute__ ((noinline, noclone))
my_nop (int i)
{
  return i;
}

int __attribute__ ((noinline, noclone))
foo (void)
{
  struct Z z;
  int i, c = get_c ();

  z.d = get_d ();
  z.s = get_s ();

  for (i = 0; i < c; i++)
    {
      z.s.i = my_nop (z.s.i);
      z.s.j = my_nop (z.s.j);
    }

  return z.s.i + z.s.j;
}

int main (int argc, char *argv[])
{
  if (foo () != 11)
    __builtin_abort ();
  return 0;
}

