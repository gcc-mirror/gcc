/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-options "-Os" } */
/* { dg-skip-if "" { ! { arm_thumb1 } } } */

extern long long madd (long long a, long long b);

long long
foo ()
{
  return madd (0x0000000100000001LL, 0x0000011100000001LL);
}

/* { dg-final { scan-assembler-not "ldr" } } */
