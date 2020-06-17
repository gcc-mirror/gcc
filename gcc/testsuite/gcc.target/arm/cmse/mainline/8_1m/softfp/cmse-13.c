/* { dg-do compile } */
/* { dg-options "-mcmse -mfloat-abi=softfp -mfpu=fpv5-d16" }  */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=softfp" } } */
/* { dg-skip-if "Skip these if testing single precision" {*-*-*} {"-mfpu=*-sp-*"} {""} } */

#include "../../../cmse-13.x"

/* Checks for saving and clearing prior to function call.  */
/* Shift on the same register as blxns.  */
/* { dg-final { scan-assembler "lsrs\t(r\[1,4-9\]|r10|fp|ip), \\1, #1.*blxns\t\\1" } } */
/* { dg-final { scan-assembler "lsls\t(r\[1,4-9\]|r10|fp|ip), \\1, #1.*blxns\t\\1" } } */
/* { dg-final { scan-assembler-not "mov\tr0, r4" } } */
/* { dg-final { scan-assembler-not "mov\tr2, r4" } } */
/* { dg-final { scan-assembler-not "mov\tr3, r4" } } */
/* { dg-final { scan-assembler "push\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" } } */
/* { dg-final { scan-assembler "vlstm\tsp" } } */
/* Check the right registers are cleared and none appears twice.  */
/* { dg-final { scan-assembler "clrm\t\{(r1, )?(r4, )?(r5, )?(r6, )?(r7, )?(r8, )?(r9, )?(r10, )?(fp, )?(ip, )?APSR\}" } } */
/* Check that the right number of registers is cleared and thus only one
   register is missing.  */
/* { dg-final { scan-assembler "clrm\t\{((r\[1,4-9\]|r10|fp|ip), ){9}APSR\}" } } */
/* Check that no cleared register is used for blxns.  */
/* { dg-final { scan-assembler-not "clrm\t\{\[^\}\]\+(r\[1,4-9\]|r10|fp|ip),\[^\}\]\+\}.*blxns\t\\1" } } */
/* { dg-final { scan-assembler "vlldm\tsp" } } */
/* { dg-final { scan-assembler "pop\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" } } */

/* Now we check that we use the correct intrinsic to call.  */
/* { dg-final { scan-assembler "blxns" } } */
