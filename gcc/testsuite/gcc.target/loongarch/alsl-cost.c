/* { dg-do compile } */
/* { dg-options "-O2 -mtune=loongarch64" } */
/* { dg-final { scan-assembler-times "alsl\\\.\[wd\]" 2 } } */

struct P
{
  long a, b;
};

struct P
t (struct P x, long n)
{
  return (struct P){.a = x.a + n * 8, .b = x.b + n * 8};
}
