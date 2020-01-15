/* { dg-do compile } */
/* { dg-options "-mcmse -mfloat-abi=hard -mfpu=fpv5-sp-d16" }  */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-skip-if "Skip these if testing double precision" {*-*-*} {"-mfpu=fpv[4-5]-d16"} {""} } */

#include "../../../cmse-5.x"

/* { dg-final { scan-assembler "vstr\tFPCXTNS, \\\[sp, #-4\\\]!" } } */
/* { dg-final { scan-assembler "vscclrm\t\{s1-s15, VPR\}" } } */
/* { dg-final { scan-assembler "clrm\t\{r0, r1, r2, r3, ip, APSR\}" } } */
/* { dg-final { scan-assembler "vldr\tFPCXTNS, \\\[sp\\\], #4" } } */
/* { dg-final { scan-assembler "bxns" } } */
