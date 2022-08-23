/* { dg-do compile } */
/* { dg-options "-Oz" } */
/* { dg-additional-options "-mregparm=2" { target ia32 } } */

int foo(int x, int y)
{
  return y;
}

long bar(long x, long y)
{
  return y;
}

/* { dg-final { scan-assembler-times "xchg" 2 } } */
