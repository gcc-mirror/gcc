/* { dg-do run } */

void abort (void);

struct S0
{
  int f0;
};
struct S1
{
  struct S0 f0;
};

struct S1 x = { {0} };
struct S1 y = { {1} };

static void
foo (struct S0 p)
{
  struct S0 *l = &y.f0;
  *l = x.f0;
  if (p.f0)
    *l = *l;
}

int
main ()
{
  foo(y.f0);
  if (y.f0.f0 != 0)
    abort ();
  return 0;
}
