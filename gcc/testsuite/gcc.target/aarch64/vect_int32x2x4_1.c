/* { dg-do compile } */
/* { dg-options "-O3 -fdump-rtl-expand" } */

#include <arm_neon.h>

uint32x2x4_t
test_1 (uint32x2x4_t a, uint32x2x4_t b)
{
   uint32x2x4_t result;

   for (unsigned index = 0; index < 4; ++index)
     result.val[index] = a.val[index] + b.val[index];

   return result;
}

/* Should not use the stack in expand.  */
/* { dg-final { scan-rtl-dump-not "virtual-stack-vars" "expand" } } */
/* Should not have to modify the stack pointer.  */
/* { dg-final { scan-assembler-not "\t(add|sub).*sp" } } */
/* Should not have to store or load anything.  */
/* { dg-final { scan-assembler-not "\t(ld|st)\[rp\]" } } */
