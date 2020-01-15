/* { dg-do compile } */
/* { dg-options "-mcmse -mfloat-abi=hard -mfpu=fpv5-d16" }  */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-skip-if "Skip these if testing single precision" {*-*-*} {"-mfpu=*-sp-*"} {""} } */

#include "../../../cmse-5.x"

/* { dg-final { scan-assembler "vstr\tFPCXTNS, \\\[sp, #-4\\\]!" } } */
/* { dg-final { scan-assembler-not "vmov\.f32\ts0, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts1, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td1, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td2, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td3, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td4, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td5, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td6, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td7, #1\.0" } } */
/* { dg-final { scan-assembler "clrm\t\{r0, r1, r2, r3, ip, APSR\}" } } */
/* { dg-final { scan-assembler "vldr\tFPCXTNS, \\\[sp\\\], #4" } } */
/* { dg-final { scan-assembler "bxns" } } */
