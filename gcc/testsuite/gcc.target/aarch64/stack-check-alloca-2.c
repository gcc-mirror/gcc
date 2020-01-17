/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-require-effective-target alloca } */

#define SIZE 0
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-not {str\s+xzr,} } } */

/* Alloca of 0 should emit no probes, boundary condition.  */
