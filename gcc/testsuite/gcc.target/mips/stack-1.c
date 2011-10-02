/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "addiu\t(\\\$sp,)?\\\$sp,\[1-9\]" } } */
/* { dg-final { scan-assembler "\tlw\t" } } */
/* { dg-final { scan-assembler-not "addiu\t(\\\$sp,)?\\\$sp,\[1-9\].*\tlw\t" } } */

int foo (int y)
{
  volatile int a = y;
  volatile int *volatile b = &a;
  return *b;
}
