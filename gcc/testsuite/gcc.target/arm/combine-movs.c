/* { dg-do compile } */
/* { dg-skip-if "" { arm_thumb1 } } */
/* { dg-options "-O" }  */

void foo (unsigned long r[], unsigned int d)
{
  int i, n = d / 32;
  for (i = 0; i < n; ++i)
    r[i] = 0;
}

/* { dg-final { scan-assembler "lsrs\tr\[0-9\]" { target arm_thumb2 } } } */
/* { dg-final { scan-assembler "movs\tr\[0-9\]" { target { ! arm_thumb2 } } } } */
