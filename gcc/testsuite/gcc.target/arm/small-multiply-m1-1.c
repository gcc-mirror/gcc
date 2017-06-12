/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-skip-if "do not override -mcpu" { *-*-* } { "-mcpu=*" "-march=*" } { "-mcpu=cortex-m1.small-multiply" } } */
/* { dg-options "-mcpu=cortex-m1.small-multiply -mthumb -O2" } */

int
test (int a)
{
  return a * 0x123456;
}

/* { dg-final { scan-assembler-not "\[\\t \]+mul" } } */
