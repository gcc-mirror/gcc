/* PR debug/45882 */
/* { dg-do run } */
/* { dg-options "-g" } */

#include "prevent-optimization.h"

extern void abort (void);
int a[1024] ATTRIBUTE_USED;
volatile short int v;

__attribute__((noinline,noclone,used)) int
foo (int i, int j)
{
  int b = i;		/* { dg-final { gdb-test .+4 "b" "7" } } */
  int c = i + 4;	/* { dg-final { gdb-test .+3 "c" "11" } } */
  int d = a[i];		/* { dg-final { gdb-test .+2 "d" "112" } } */
  int e = a[i + 6];	/* { dg-final { gdb-test .+1 "e" "142" } } */
  ++v;
  return ++j;
}

int
main (void)
{
  int l;
  asm ("" : "=r" (l) : "0" (7));
  a[7] = 112;
  a[7 + 6] = 142;
  if (foo (l, 7) != 8)
    abort ();
  return l - 7;
}
