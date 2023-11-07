/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

extern void abort (void);

void __attribute__((noinline,noclone))
test(int *__restrict__ a, int *__restrict__ b)
{
  a[0] = b[0];
  a[1] = b[1];
  a[2] = b[2];
  a[3] = b[3];
  a[5] = 0;
  a[6] = 0;
  a[7] = 0;
  a[8] = 0;
}

int main()
{
  int a[9];
  int b[4];
  b[0] = 1;
  __asm__ volatile ("");
  b[1] = 2;
  __asm__ volatile ("");
  b[2] = 3;
  __asm__ volatile ("");
  b[3] = 4;
  __asm__ volatile ("");
  a[4] = 7;
  check_vect ();
  test(a, b);
  if (a[0] != 1)
    abort ();
  __asm__ volatile ("");
  if (a[1] != 2)
    abort ();
  __asm__ volatile ("");
  if (a[2] != 3)
    abort ();
  __asm__ volatile ("");
  if (a[3] != 4)
    abort ();
  __asm__ volatile ("");
  if (a[4] != 7)
    abort ();
  __asm__ volatile ("");
  if (a[5] != 0)
    abort ();
  __asm__ volatile ("");
  if (a[6] != 0)
    abort ();
  __asm__ volatile ("");
  if (a[7] != 0)
    abort ();
  __asm__ volatile ("");
  if (a[8] != 0)
    abort ();
  __asm__ volatile ("");
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "slp2" { target { vect_element_align || vect_hw_misalign } } } } */
