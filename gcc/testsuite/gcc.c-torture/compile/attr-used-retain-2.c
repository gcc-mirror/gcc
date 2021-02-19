/* { dg-do compile } */
/* { dg-require-effective-target R_flag_in_section } */
/* { dg-final { scan-assembler-not ".text.used_fn,\"axR\"" } } */
/* { dg-final { scan-assembler-not ".text.used_fn2,\"axR\"" } } */
/* { dg-final { scan-assembler-not ".bss.used_bss,\"awR\"" } } */
/* { dg-final { scan-assembler-not ".bss.used_bss2,\"awR\"" } } */
/* { dg-final { scan-assembler-not ".data.used_data,\"awR\"" } } */
/* { dg-final { scan-assembler-not ".data.used_data2,\"awR\"" } } */
/* { dg-final { scan-assembler-not ".rodata.used_rodata,\"aR\"" } } */
/* { dg-final { scan-assembler-not ".rodata.used_rodata2,\"aR\"" } } */
/* { dg-final { scan-assembler-not ".bss.used_lcomm,\"awR\"" { target arm-*-* } } } */
/* { dg-final { scan-assembler-not ".bss.used_lcomm2,\"awR\"" { target arm-*-* } } } */
/* { dg-final { scan-assembler-not ".data.used_foo_sec,\"awR\"" } } */
/* { dg-options "-ffunction-sections -fdata-sections" } */

#include "attr-used-retain-1.c"
