/* Test for switching architectures during compilation.  */
/* { dg-skip-if "instruction not valid on thumb" { *-*-* } { "-mthumb" } { "" } } */
/* { dg-do assemble } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-additional-options "-Wall -O2 -march=armv4t -std=gnu99 -marm" } */

#pragma GCC target ("arch=armv5te")
void cpu_has_iwmmxt (void)
{
   int lo;
   int hi;
   __asm__ __volatile__ (
      "mcrr   p0, 0, %2, %3, c0\n"
      : "=r" (lo), "=r" (hi)
      : "r" (0), "r" (0x100));
}

