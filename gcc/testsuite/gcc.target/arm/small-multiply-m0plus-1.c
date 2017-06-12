/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-skip-if "do not override -mcpu" { *-*-* } { "-mcpu=*" "-march=*" } { "-mcpu=cortex-m0plus.small-multiply" } } */
/* { dg-options "-mcpu=cortex-m0plus.small-multiply -mthumb -O2" } */

int
test (int a)
{
  return a * 0x123456;
}

/* { dg-final { scan-assembler-not "\[\\t \]+mul" } } */
