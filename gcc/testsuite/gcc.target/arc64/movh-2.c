/* { dg-do compile } */
/* { dg-options "-O2" } */

/* check "movh-1.c" for further details. */

/* assign a register to register */
short foo(short a, short b)
{
  return b;
}
/* { dg-final { scan-assembler "sexh_s\\s+r\[0-9\]+,r\[0-9\]+" } } */
