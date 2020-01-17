/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-require-effective-target alloca } */

#define SIZE 63.5 * 1024
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-times {str\s+xzr, \[sp, 1024\]} 1 } } */

/* Alloca is more than 1kB, but less than guard-size, 1 probe expected at 1kB
   offset.  */
