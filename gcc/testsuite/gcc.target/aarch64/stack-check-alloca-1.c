/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-require-effective-target alloca } */

#define SIZE y
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-times {str\s+xzr, \[sp, 1024\]} 2 } } */
/* { dg-final { scan-assembler-times {str\s+xzr, \[sp\]} 1 } } */

/* Dynamic alloca, expect loop, and 2 probes with 1kB offset and 1 at sp.
   1st probe is inside the loop for the full guard-size allocations, second
   probe is for the case where residual is zero and the final probe for when
   residiual is > 1024 bytes.  */
