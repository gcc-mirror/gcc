// Submitted by Erik Rozendaal <dlr@acm.org>
// Test case for GNATS bug 787.
// { dg-do run }

#include <stdio.h>
#include <stdlib.h>

static int calls;

int &foo (int &arg)
{
  calls++;
  arg=0;
  return arg;
}

int &identity (int &x)
{
  return x;
}

int main()
{
  int a;

  calls = 0;
  int &b = ++foo (a);
  if (calls > 1)
    abort ();
  if (&a != &b)
    abort ();
  if (a != 1)
    abort ();

  calls = 0;
  int &c = ++identity (++foo (a));
  if (calls > 1)
    abort ();
  if (&a != &c)
    abort ();
  if (a != 2)
    abort ();

  exit (0);
}
