/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-skip-if "" { ! { arm_thumb1 } } } */

int
mymul3 (int x)
{
  return x * 0x555;
}

/* { dg-final { scan-assembler "mul\[\\t \]*r.,\[\\t \]*r." } } */
