/* { dg-do compile } */
/* { dg-options "-mcmse -mfloat-abi=softfp -mfpu=fpv5-d16" }  */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=softfp" } } */
/* { dg-skip-if "Skip these if testing single precision" {*-*-*} {"-mfpu=*-sp-*"} {""} } */

#include "../../../cmse-5.x"

/* { dg-final { scan-assembler "__acle_se_foo:" } } */
/* { dg-final { scan-assembler-not "mov\tr0, lr" } } */
/* { dg-final { scan-assembler "mov\tr1, lr" } } */
/* { dg-final { scan-assembler "mov\tr2, lr" } } */
/* { dg-final { scan-assembler "mov\tr3, lr" } } */
/* { dg-final { scan-assembler "vmov\.f64\td0, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td1, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td2, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td3, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td4, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td5, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td6, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td7, #1\.0" } } */
/* { dg-final { scan-assembler "msr\tAPSR_nzcvq, lr" { target { ! arm_dsp } } } } */
/* { dg-final { scan-assembler "msr\tAPSR_nzcvqg, lr" { target arm_dsp } } } */
/* { dg-final { scan-assembler "push\t{r4}" } } */
/* { dg-final { scan-assembler "vmrs\tip, fpscr" } } */
/* { dg-final { scan-assembler "movw\tr4, #65376" } } */
/* { dg-final { scan-assembler "movt\tr4, #4095" } } */
/* { dg-final { scan-assembler "and\tip, r4" } } */
/* { dg-final { scan-assembler "vmsr\tfpscr, ip" } } */
/* { dg-final { scan-assembler "pop\t{r4}" } } */
/* { dg-final { scan-assembler "mov\tip, lr" } } */
/* { dg-final { scan-assembler "bxns" } } */
