/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-skip-if "Test is specific to cortex-m0.small-multiply" { arm*-*-* } { "-mcpu=*" } { "-mcpu=cortex-m0.small-multiply" } } */
/* { dg-options "-mcpu=cortex-m0.small-multiply -mthumb -Os" } */

int
test (int a)
{
  return a * 0x13;
}

/* { dg-final { scan-assembler-not "\[\\t \]+mul" } } */
