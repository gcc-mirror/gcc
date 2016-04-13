/* Check that attribute target arm is recognized.  */
/* { dg-do compile } */
/* { dg-skip-if "" arm_cortex_m } */
/* { dg-final { scan-assembler "\\.arm" } } */
/* { dg-final { scan-assembler-not "\\.thumb_func" } } */

int __attribute__((target("arm")))
foo(int a)
{
  return a ? 1 : 5;
}

