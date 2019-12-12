/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 63 * 1024 + 512
#include "stack-check-prologue.h"

/* { dg-final { scan-assembler-times {str\s+xzr, \[sp, 1024\]} 1 } } */

/* SIZE is more than guard-size - 1Kb and remainder is less than 1kB,
   1 probe expected.  */
