/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection -funwind-tables -fno-stack-protector" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 128*1024
#include "stack-check-prologue.h"

/* { dg-final { scan-assembler-times {\.cfi_def_cfa [0-9]+, 131072} 1 } } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_register 2} 1 } } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 0} 1 } } */

/* Checks that the CFA notes are correct for every sp adjustment.  */
