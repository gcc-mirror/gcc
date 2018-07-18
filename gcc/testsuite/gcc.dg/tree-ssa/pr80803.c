/* { dg-do run { target int32plus } } */
/* { dg-options "-O" } */

struct S0
{
  unsigned a : 15;
  int b;
  int c;
};

struct S1
{
  struct S0 s0;
  int e;
};

struct Z
{
  char c;
  int z;
} __attribute__((packed));

union U
{
  struct S1 s1;
  struct Z z;
};


int __attribute__((noinline, noclone))
return_zero (void)
{
  return 0;
}

volatile union U gu;
struct S0 gs;

int __attribute__((noinline, noclone))
check_outcome ()
{
  if (gs.a != 6
      || gs.b != 80000)
    __builtin_abort ();
}

int
main (int argc, char *argv[])
{
  union U u;
  struct S1 m,n;
  struct S0 l;

  if (return_zero ())
    u.z.z = 20000;
  else
    {
      u.s1.s0.a = 6;
      u.s1.s0.b = 80000;
      u.s1.e = 2;

      n = u.s1;
      m = n;
      m.s0.c = 0;
      l = m.s0;
      gs = l;
    }

  gu = u;
  check_outcome ();
  return 0;
}
