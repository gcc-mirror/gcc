/* PR target/pr70799 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=slm -fno-split-wide-types -mno-stackrealign" } */
/* { dg-final { scan-assembler "psllq" } } */
/* { dg-final { scan-assembler "psrlq" } } */

unsigned long long a, b, c;

void test1 (void)
{
  a = (b << 55) | c;
}

void test2 (void)
{
  a = (b >> 55) | c;
}
