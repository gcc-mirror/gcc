/* { dg-do compile } */
/* { dg-options "-O2" } */

/* check "movd-1.c" for further details. */

/* assign a register to register */
int foo(int a, int b)
{
  return b;
}
/* { dg-final { scan-assembler "mov_s\\s+r\[0-9\]+,r\[0-9\]+" } } */
