/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -fstack-clash-protection -funwind-tables -fno-stack-protector" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#include "stack-check-prologue-16.c"

/* Checks that the CFA notes are correct for every sp adjustment, but we also
   need to make sure we can unwind correctly before the frame is set up.  So
   check that we're emitting t3 with a copy of sp an setting the CFA there.  */

/* { dg-final { scan-assembler-times {mv\tt3,sp} 1 } } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa [0-9]+, 0} 1 } } */
/* { dg-final { scan-assembler-times {\.cfi_escape 0xf,0x9,0x72,0,0x92,0xa2,0x38,0,0x3a,0x1e,0x22} 1 } } */
/* { dg-final { scan-assembler-times {\.cfi_escape 0xf,0xa,0x72,0,0x92,0xa2,0x38,0,0x9,0xf6,0x1e,0x22} 1 } } */
