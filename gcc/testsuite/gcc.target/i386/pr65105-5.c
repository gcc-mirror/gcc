/* PR target/pr65105 */
/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -march=core-avx2 -mno-stackrealign" } */
/* { dg-final { scan-assembler "pandn" } } */
/* { dg-final { scan-assembler "pxor" } } */
/* { dg-final { scan-assembler "ptest" } } */

struct S1
{
  unsigned long long a;
  unsigned long long b;
};

void
test (int p1, unsigned long long p2, int p3, struct S1 *p4)
{
  int i;

  for (i = 0; i < p1; i++)
    if ((p4[i].a & p2) != p2)
      p4[i].a ^= (1ULL << p3);
}
