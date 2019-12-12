/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 128
#include "stack-check-prologue.h"

/* { dg-final { scan-assembler-times {str\s+xzr,} 0 } } */

/* SIZE is smaller than guard-size - 1Kb so no probe expected.  */
