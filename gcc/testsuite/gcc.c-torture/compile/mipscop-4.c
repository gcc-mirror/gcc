/* { dg-do compile { target mips*-*-* } } */

#include <stdio.h>
register unsigned long c3r1 asm ("$c3r1"), c3r2 asm ("$c3r2");

extern unsigned long b, c;

void __attribute__ ((nomips16))
foo ()
{
  unsigned long a, d;

  c3r1 = a;
  b = c3r1;

  c3r2 = c;
  d = c3r1;
  printf ("%d\n", d);
}

