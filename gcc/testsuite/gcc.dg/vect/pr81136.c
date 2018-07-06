/* { dg-do compile } */

#include "tree-vect.h"

#if VECTOR_BITS > 256
#define ALIGNMENT (VECTOR_BITS / 8)
#else
#define ALIGNMENT 32
#endif

struct __attribute__((aligned (ALIGNMENT)))
{
  char misaligner;
  int foo[100];
  int bar[100];
} *a;

void
fn1 (int n)
{
  int *b = a->foo;
  for (int i = 0; i < n; i++)
    a->bar[i] = b[i];
}

/* { dg-final { scan-tree-dump-not "Unknown misalignment" "vect" } } */
