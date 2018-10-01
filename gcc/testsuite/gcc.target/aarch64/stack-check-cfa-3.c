/* { dg-do compile } */
/* { dg-options "-O3 -fopenmp-simd -march=armv8-a+sve -fstack-clash-protection --param stack-clash-protection-guard-size=16 -funwind-tables" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#include "stack-check-prologue-16.c"

/* Checks that the CFA notes are correct for every sp adjustment, but we also
   need to make sure we can unwind correctly before the frame is set up.  So
   check that we're emitting r15 with a copy of sp an setting the CFA there.  */

/* { dg-final { scan-assembler-times {mov\tx15, sp} 1 } } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_register 15} 1 } } */
/* { dg-final { scan-assembler-times {\.cfi_escape 0xf,0xc,0x8f,0,0x92,0x2e,0,.*} 1 } } */
