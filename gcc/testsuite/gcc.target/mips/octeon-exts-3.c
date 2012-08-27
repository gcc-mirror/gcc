/* { dg-do compile } */
/* { dg-options "-march=octeon -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-times "\texts\t" 3 } } */

struct foo
{
  unsigned long long a:10;
  unsigned long long b:32;
  unsigned long long c:22;
};

NOMIPS16 unsigned
f (struct foo s)
{
  return s.b;
}

struct bar
{
  unsigned long long a:15;
  unsigned long long b:48;
  unsigned long long c:1;
};

NOMIPS16 int
g (struct bar s)
{
  return (int) s.b;
}

NOMIPS16 int
h (int i)
{
  return (i << 4) >> 24;
}
