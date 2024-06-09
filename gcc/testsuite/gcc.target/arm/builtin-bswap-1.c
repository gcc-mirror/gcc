/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6t2_ok } */
/* { dg-add-options arm_arch_v6t2 } */
/* This test depends on if-conversion creating the conditional forms of
   of the instructions.  Add an -mtune option known to facilitate that.  */
/* { dg-additional-options "-O2 -mtune=cortex-a53" } */
/* { dg-final { scan-assembler-not "orr\[ \t\]" } } */
/* { dg-final { scan-assembler-times "revsh\\t" 1 } }  */
/* { dg-final { scan-assembler-times "revshne\\t" 1 } }  */
/* { dg-final { scan-assembler-times "rev16\\t" 1 } }  */
/* { dg-final { scan-assembler-times "rev16ne\\t" 1 } }  */
/* { dg-final { scan-assembler-times "rev\\t" 2 } }  */
/* { dg-final { scan-assembler-times "revne\\t" 2 } }  */

#include "builtin-bswap.x"
