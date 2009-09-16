/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fPIC" } */

volatile _Decimal32 d;
volatile int i;

void foo()
{
  d += i;
  d += i;
}
