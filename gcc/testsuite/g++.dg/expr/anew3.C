// { dg-do run { xfail *-*-* } }
// XFAILed until PR2123 is fixed
// PR 11228: array operator new, with zero-initialization and a variable sized array.
// Regression test for PR 
// Author: Matt Austern <austern@apple.com>

#include <new>
#include <stdlib.h>
#include <string.h>

struct X
{
  int a;
  double b;
};

X* allocate(int n)
{
  void *p;
  p = malloc(n * sizeof (X));
  memset (p, 0xff, n * sizeof(X));
  return new (p) X[n]();
}

int main()
{
  const int n = 17;
  X* p = allocate(n);
  for (int i = 0; i < n; ++i)
    if (p[i].a != 0 || p[i].b != 0.0)
      abort ();
  exit (0);
}
