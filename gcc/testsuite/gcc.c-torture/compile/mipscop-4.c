/* { dg-do compile { target mips*-*-* } } */

#ifndef __mips16
register unsigned long c3r1 asm ("$c3r1"), c3r2 asm ("$c3r2");

extern unsigned long b, c;

void
foo ()
{
  unsigned long a, d;

  c3r1 = a;
  b = c3r1;

  c3r2 = c;
  d = c3r1;
  printf ("%d\n", d);
}
#endif
