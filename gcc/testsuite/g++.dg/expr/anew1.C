// { dg-do run }
// PR 11228: array operator new, with zero-initialization and a variable sized array.
// Regression test for PR 
// Author: Matt Austern <austern@apple.com>


#include <new>
#include <stdlib.h>
#include <string.h>

int* allocate(int n)
{
  void *p;
  p = malloc(n * sizeof (int));
  memset (p, 0xff, n * sizeof(int));
  return new (p) int[n]();
}

int main()
{
  const int n = 17;
  int* p = allocate(n);
  for (int i = 0; i < n; ++i)
    if (p[i] != 0)
      abort ();
  exit (0);
}
