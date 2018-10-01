/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 127 * 1024
#include "stack-check-prologue.h"

/* { dg-final { scan-assembler-times {str\s+xzr, \[sp, 1024\]} 2 } } */

/* SIZE is more than 1x guard-size and remainder equal than guard-size - 1Kb,
   2 probe expected, unrolled, no loop.  */
