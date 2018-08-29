/* PR target/pr70799 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=slm -mavx512vl -mno-stackrealign" } */
/* { dg-final { scan-assembler "psllq" } } */
/* { dg-final { scan-assembler "psraq" } } */

long long a, b;

void test1 (int c)
{
  a = b << c;
}

void test2 (int c)
{
  a = b >> c;
}
