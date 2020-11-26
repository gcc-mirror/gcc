/* PR target/97873 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse3 -mtune=generic" } */

short test_absw (short x)
{
  return (x < 0) ? -x : x;
}

short test_sminw (short x, short y)
{
  return (x < y) ? x : y;
}

/* { dg-final { scan-assembler-not "movswl" } } */

char test_absb (char x)
{
  return (x < 0) ? -x : x;
}

char test_sminb (char x, char y)
{
  return (x < y) ? x : y;
}

/* { dg-final { scan-assembler-not "movsbl" } } */
