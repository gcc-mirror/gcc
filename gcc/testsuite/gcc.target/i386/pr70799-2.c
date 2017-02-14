/* PR target/pr70799 */
/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -march=slm -mno-stackrealign" } */
/* { dg-final { scan-assembler "psllq" } } */
/* { dg-final { scan-assembler "psrlq" } } */

unsigned long long a, b;

void test1 (void)
{
  a = b << 21;
}

void test2 (void)
{
  a = b >> 21;
}
