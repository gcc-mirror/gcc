/* { dg-do compile } */
/* { dg-options "-mcmse" } */

#include "../cmse-13.x"

/* Checks for saving and clearing prior to function call.  */
/* { dg-final { scan-assembler "lsrs\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "lsls\tr4, r4, #1" } } */
/* { dg-final { scan-assembler-not "movs\tr0, r4" } } */
/* { dg-final { scan-assembler "\n\tmovs\tr1, r4" } } */
/* { dg-final { scan-assembler-not "\n\tmovs\tr2, r4\n\tmovs\tr3, r4" } } */
/* { dg-final { scan-assembler-not "vmov" } } */
/* { dg-final { scan-assembler-not "vmsr" } } */

/* Now we check that we use the correct intrinsic to call.  */
/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" } } */

