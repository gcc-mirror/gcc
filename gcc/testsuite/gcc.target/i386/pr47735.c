/* PR middle-end/47735 */
/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

unsigned
mulh (unsigned a, unsigned b)
{
  unsigned long long l __attribute__ ((aligned (32)))
    = ((unsigned long long) a * (unsigned long long) b) >> 32;
  return l;
}

/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
