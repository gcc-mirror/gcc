/* PR target/36473 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=core2" } */
/* { dg-additional-options "-mregparm=2" { target ia32 } } */

int test(unsigned long x, unsigned long n)
{
  return !(x & ( (long)0x01 << n ));
}

/* { dg-final { scan-assembler "btl\[ \t\]" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "btq\[ \t\]" { target lp64 } } } */
