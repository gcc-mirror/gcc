/* PR target/61948 */
/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-O2 -mthumb" } */
/* { dg-add-options arm_neon } */

long long f (long long *c)
{
  long long t = c[0];
  asm ("nop" : : : "r0", "r3", "r4", "r5",
		   "r6", "r7", "r8", "r9",
		   "r10", "r11", "r12", "memory");
  return t >> 1;
}

