/* { dg-do compile { target mips*-*-* } } */

#ifndef __mips16
register unsigned int c3r1 asm ("$c3r1");

extern unsigned int b, c;

void
foo ()
{
  unsigned int a, d;

  c3r1 = a;
  b = c3r1;

  c3r1 = c;
  d = c3r1;
  printf ("%d\n", d);
}
#endif
