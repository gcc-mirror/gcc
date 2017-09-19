/* { dg-do compile } */
/* { dg-options "-O2 -mtune=core2" } */
/* { dg-additional-options "-mregparm=2" { target ia32 } } */

int test (unsigned long x, unsigned long n)
{
  n &= 0x3f;

  return !(x & ((long)0x01 << n));
}

/* { dg-final { scan-assembler-not "and\[lq\]\[ \t\]" } } */
