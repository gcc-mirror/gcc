/* Test for switching architectures during compilation.  */
/* { dg-skip-if "instruction not valid on thumb" { *-*-* } { "-mthumb" } { "" } } */
/* { dg-do assemble } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-additional-options "-Wall -O2 -march=armv5te -std=gnu99 -marm" } */

#pragma GCC target ("arch=armv6")
int test_assembly (int hi, int lo)
{
   int res;
   __asm__ __volatile__ (
      "uxtah   %0, %1, %2\n"
      : "=r" (res)
      : "r" (hi),  "r" (lo));
   return res;
}

