/* { dg-do run { xfail { cris-*-* sparc*-*-* } } } */
/* { dg-options "-O2" } */

#include <inttypes.h>

struct U {
    int M0;
    int M1;
} __attribute ((aligned (16)));

volatile struct U p0 = {1, 0};

void __attribute__ ((noinline))
foo (struct U p)
{

  volatile intptr_t mask = 0b1111;
  volatile int dummy[2];
  struct U p1 = p;
  dummy[1] = p.M0;

  if ((intptr_t)(&p1) & mask)
    __builtin_abort ();
  if ((intptr_t)(&p) & mask)
    __builtin_abort ();

  if (p1.M0 != dummy[1])
    __builtin_abort ();
  if (p1.M1 != p.M1)
    __builtin_abort ();
}

int
main ()
{
  foo (p0);
  return 0;
}
