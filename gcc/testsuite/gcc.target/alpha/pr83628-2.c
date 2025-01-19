/* PR target/83628 */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

int
s4l (int a, int b)
{
  return a * 4 + b;
}

int
s8l (int a, int b)
{
  return a * 8 + b;
}

long
s4q (long a, long b)
{
  return a * 4 + b;
}

long
s8q (long a, long b)
{
  return a * 8 + b;
}

/* { dg-final { scan-assembler-not "\[ \t\]add\[ql\]" } } */
