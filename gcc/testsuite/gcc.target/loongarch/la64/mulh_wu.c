/* { dg-do compile { target { loongarch64*-*-* } } } */
/* { dg-options "-O3 -mabi=lp64d" } */
/* { dg-final { scan-assembler "\tmulh.wu" } } */
/* { dg-final { scan-assembler-not "\tlu32i.d" } } */

unsigned int
test (unsigned int *a)
{
  return *a / 60;
}
