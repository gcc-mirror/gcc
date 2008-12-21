/* { dg-do compile } */
/* { dg-options "-O -march=octeon -meb" } */
/* { dg-final { scan-assembler-times "\texts\t" 4 } } */

struct bar
{
  unsigned long long a:1;
  long long b:14;
  unsigned long long c:48;
  long long d:1;
};

NOMIPS16 int
f1 (struct bar *s, int a)
{
  return (int) s->b + a;
}

NOMIPS16 char
f2 (struct bar *s)
{
  return s->d + 1;
}

NOMIPS16 int
f3 ()
{
  struct bar s;
  asm ("" : "=r"(s));
  return (int) s.b + 1;
}

NOMIPS16 long long
f4 (struct bar *s)
{
  return s->d;
}
