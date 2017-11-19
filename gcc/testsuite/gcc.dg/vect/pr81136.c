/* { dg-do compile } */

#include "tree-vect.h"

struct __attribute__((aligned (VECTOR_BITS / 8)))
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
