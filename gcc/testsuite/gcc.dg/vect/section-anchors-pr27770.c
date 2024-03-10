/* { dg-require-effective-target section_anchors } */ 

#include <stdarg.h>
#include "tree-vect.h"

short x;
static short f[100] = {0};
int
bar (void)
{
  return f[0];
}
void
foo (void)
{
  int i;
  for (i = 0; i < 100; i++)
    f[i]++;
}
int main (void)
{
  int i;
  check_vect ();
  foo ();
#pragma GCC novector
  for (i = 0; i < 100; i++)
    if (f[i]!=1) 
      abort ();
  return 0;
}

