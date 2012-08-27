/* { dg-do compile } */
/* { dg-options "-march=octeon" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-times "\tbaddu\t" 4 } } */
/* { dg-final { scan-assembler-not "\tandi\t" } } */

NOMIPS16 unsigned char
g (long long a, long long b)
{
  return a + b;
}

NOMIPS16 unsigned long long
h (unsigned long long a, unsigned long long b)
{
  unsigned char c = a + b;
  return c;
}

NOMIPS16 long long
ff (long long a, long long b)
{
  unsigned char c = a + b;
  return c;
}

NOMIPS16 int
gg (int a, int b)
{
  return (a + b) & 0xff;
}
