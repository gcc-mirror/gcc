// { dg-do run { xfail *-*-* } }
// XFAILed until PR2123 is fixed
// PR 11228: array operator new, with zero-initialization and a variable sized array.
// Regression test for PR 
// Author: Matt Austern <austern@apple.com>

#include <new>
#include <stdlib.h>
#include <string.h>

struct B
{
  B();
  int n;
};

B::B()
{
  n = 137;
}


struct D : public B
{
  double x;
};


D* allocate(int n)
{
  void *p;
  p = malloc(n * sizeof (D));
  memset (p, 0xff, n * sizeof(D));
  return new (p) D[n]();
}

int main()
{
  const int n = 17;
  D* p = allocate(n);
  for (int i = 0; i < n; ++i)
    if (p[i].n != 137 || p[i].x != 0)
      abort ();
  exit (0);
}
