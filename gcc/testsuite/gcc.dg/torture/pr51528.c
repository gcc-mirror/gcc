/* { dg-do run } */
/* { dg-options "-fno-early-inlining" } */

extern void abort (void);

union U
{
  int i;
  _Bool b;
};

_Bool gb;

void  __attribute__ ((noinline))
use_bool (union U u)
{
  gb = u.b;
}

union U
bar (void)
{
  union U u;
  u.i = 0xFFFE;
  return u;
}

union U  __attribute__ ((noinline))
foo (void)
{
  union U u,v;

  u.b = 1;
  use_bool (u);
  u = bar ();

  return u;
}

int main (int argc, char **argv)
{
  union U u = foo ();
  if (u.i != 0xFFFE)
    abort ();
  return 0;
}
