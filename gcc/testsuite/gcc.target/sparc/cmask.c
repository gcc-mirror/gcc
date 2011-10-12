/* { dg-do compile } */
/* { dg-options "-mvis3" } */

void test_cm8 (long x)
{
  __builtin_vis_cmask8 (x);
}

void test_cm16 (long x)
{
  __builtin_vis_cmask16 (x);
}

void test_cm32 (long x)
{
  __builtin_vis_cmask32 (x);
}

/* { dg-final { scan-assembler "cmask8\t%" } } */
/* { dg-final { scan-assembler "cmask16\t%" } } */
/* { dg-final { scan-assembler "cmask32\t%" } } */
