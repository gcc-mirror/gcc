/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16 -funwind-tables" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 128*1024
#include "stack-check-prologue.h"

/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 65536} 1 } } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 131072} 1 } } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 0} 1 } } */

/* Checks that the CFA notes are correct for every sp adjustment.  */
