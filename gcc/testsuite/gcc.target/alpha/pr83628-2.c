/* PR target/83628 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
s4l (int a, int b)
{
  return a + b * 4;
}

int
s8l (int a, int b)
{
  return a + b * 8;
}

long
s4q (long a, long b)
{
  return a + b * 4;
}

long
s8q (long a, long b)
{
  return a + b * 8;
}

/* { dg-final { scan-assembler-not "\[ \t\]add\[ql\]" } } */
