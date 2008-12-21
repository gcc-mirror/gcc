/* { dg-do compile } */
/* { dg-options "-march=octeon" } */
/* { dg-final { scan-assembler "\texts\t" } } */

struct foo
{
  long long a:3;
  long long b:23;
  long long c:38;
};

NOMIPS16 int
f (struct foo s)
{
  return s.b;
}
