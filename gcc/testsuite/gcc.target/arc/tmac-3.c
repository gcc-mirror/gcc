/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=hs38 -Os" } */

/* The compiler will assign r1r2 as a DI register, but it doesn't fit
   the macd operation, hence we need to fall back on the mac
   instruction.  */
typedef long long myint64_t;

extern int d (int, myint64_t);
int b (int c)
{
  int x = (int) d;
  d (c, (myint64_t)x * 2 + 1);
}

/* { dg-final { scan-assembler "mac\\\s+r1" } } */
