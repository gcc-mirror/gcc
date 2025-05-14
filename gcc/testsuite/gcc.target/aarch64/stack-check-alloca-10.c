/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 127.5 * 64 * 1024
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-times {str\s+xzr, \[sp, 1024\]} 2 } } */

/* Large alloca of an amount which isn't a multiple of a guard-size, and
   residiual is more than 1kB.  Loop expected with one 1Kb probe offset and
   one residual probe at offset 1kB.  */
