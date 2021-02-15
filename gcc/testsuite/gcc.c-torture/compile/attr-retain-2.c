/* { dg-do compile { target R_flag_in_section } } */
/* { dg-final { scan-assembler ".text.used_fn,\"axR\"" } } */
/* { dg-final { scan-assembler ".text.used_fn2,\"axR\"" } } */
/* { dg-final { scan-assembler ".bss.used_bss,\"awR\"" } } */
/* { dg-final { scan-assembler ".bss.used_bss2,\"awR\"" } } */
/* { dg-final { scan-assembler ".data.used_data,\"awR\"" } } */
/* { dg-final { scan-assembler ".data.used_data2,\"awR\"" } } */
/* { dg-final { scan-assembler ".rodata.used_rodata,\"aR\"" } } */
/* { dg-final { scan-assembler ".rodata.used_rodata2,\"aR\"" } } */
/* { dg-final { scan-assembler ".bss.used_lcomm,\"awR\"" { target arm-*-* } } } */
/* { dg-final { scan-assembler ".bss.used_lcomm2,\"awR\"" { target arm-*-* } } } */
/* { dg-final { scan-assembler ".data.used_foo_sec,\"awR\"" } } */
/* { dg-options "-ffunction-sections -fdata-sections" } */

#include "attr-retain-1.c"
