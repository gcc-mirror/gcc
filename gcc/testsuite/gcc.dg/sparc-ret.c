/* Copyright (C) 2001  Free Software Foundation.

   Making sure that Ultrasparc return instructions do not read
   below the stack.  */

/* { dg-do compile { target sparc*-*-* } } */
/* { dg-options "-mcpu=ultrasparc -O -m32" } */


int bar (int a, int b, int c, int d, int e, int f, int g, int h)
{
  int res;

  toto (&res);
  return h;
}
/* { dg-final { scan-assembler "return\[ \t\]*%i7\\+8\n\[^\n\]*ld\[ \t\]*\\\[%sp\\+96\\\]" } } */

int bar2 ()
{
  int res;

  toto (&res);
  return res;
}
/* { dg-final { scan-assembler "return\[ \t\]*%i7\\+8\n\[^\n\]*nop" } } */

