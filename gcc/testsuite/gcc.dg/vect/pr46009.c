/* PR tree-optimization/46009 */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

int a[1024] __attribute__((aligned));
int b[1024] __attribute__((aligned));
int c[1024] __attribute__((aligned));
int d[1024] __attribute__((aligned));
int e[1024] __attribute__((aligned));

void __attribute__((noinline))
foo (void)
{
  int i, g;
  for (i = 0; i < 1024; i++)
    {
      g = a[i] + b[i] + c[i] * d[i];;
      e[i] = g < 10 ? 1 : g;
    }
}

void __attribute__((noinline))
bar (void)
{
  int i, g;
  for (i = 0; i < 1024; i++)
    {
      g = a[i] + b[i] + c[i] * d[i];;
      if (g < 10)
	e[i] = 1;
      else
	e[i] = g;
    }
}

int
main (void)
{
  int i;
  check_vect ();
  for (i = 0; i < 1024; i++)
    {
      asm volatile ("" : "+r" (i));
      a[i] = i % 10;
      b[i] = i % 10;
      c[i] = 1;
      d[i] = -1;
      e[i] = -1;
    }
  foo ();
  for (i = 0; i < 1024; i++)
    {
      int g;
      asm volatile ("" : "+r" (i));
      g = 2 * (i % 10) - 1;
      if (e[i] != (g < 10 ? 1 : g))
	abort ();
      e[i] = -1;
    }
  bar ();
  for (i = 0; i < 1024; i++)
    {
      int g;
      asm volatile ("" : "+r" (i));
      g = 2 * (i % 10) - 1;
      if (e[i] != (g < 10 ? 1 : g))
	abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target vect_int_mult } } } */
