/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-require-effective-target alloca } */

#define SIZE 127 * 64 * 1024
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-times {str\s+xzr, \[sp, 1024\]} 1 } } */

/* Large alloca of a constant amount which is a multiple of a guard-size,
   no residiual.  Loop expected with one 1Kb probe offset and no residual probe
   because residual is at compile time known to be zero.  */
