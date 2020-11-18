/* { dg-require-effective-target vect_double } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */

#include "tree-vect.h"

double x[1024], y[1024];

void __attribute__((noipa)) foo()
{
  for (int i = 0; i < 512; ++i)
    {
      x[2*i] = y[i];
      x[2*i+1] = y[i];
    }
}

void __attribute__((noipa)) bar()
{
  for (int i = 0; i < 512; ++i)
    {
      x[2*i] = y[2*i];
      x[2*i+1] = y[2*i];
    }
}

void __attribute__((noipa)) baz()
{
  for (int i = 0; i < 512; ++i)
    {
      x[2*i] = y[511-i];
      x[2*i+1] = y[511-i];
    }
}

void __attribute__((noipa)) boo()
{
  for (int i = 0; i < 512; ++i)
    {
      x[2*i] = y[2*(511-i)];
      x[2*i+1] = y[2*(511-i)];
    }
}

int 
main ()
{
  check_vect ();

  for (int i = 0; i < 1024; ++i)
    {
      x[i] = 0;
      y[i] = i;
      __asm__ volatile ("");
    }

  foo ();
  for (int i = 0; i < 1024; ++i)
    if (x[i] != y[i/2])
      abort ();

  for (int i = 0; i < 1024; ++i)
    {
      x[i] = 0;
      __asm__ volatile ("");
    }

  bar ();
  for (int i = 0; i < 1024; ++i)
    if (x[i] != y[2*(i/2)])
      abort ();

  for (int i = 0; i < 1024; ++i)
    {
      x[i] = 0;
      __asm__ volatile ("");
    }

  baz ();
  for (int i = 0; i < 1024; ++i)
    if (x[i] != y[511 - i/2])
      abort ();

  for (int i = 0; i < 1024; ++i)
    {
      x[i] = 0;
      __asm__ volatile ("");
    }

  boo ();
  for (int i = 0; i < 1024; ++i)
    if (x[i] != y[2*(511 - i/2)])
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { xfail vect_load_lanes } } } */
