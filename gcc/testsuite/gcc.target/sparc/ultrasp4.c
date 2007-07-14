/* Simplified from PR target/5309.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=ultrasparc" } */

extern long bar (unsigned int);

long
foo (long x, unsigned int y)
{
  return *(((long *) (bar (y) - 1)) + 1 + (x >> 2) % 359);
}
