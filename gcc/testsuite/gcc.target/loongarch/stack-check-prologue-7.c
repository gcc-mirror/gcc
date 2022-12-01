/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-skip-if "" { *-*-* } { "-fstack-check" } { "" } } */

#define SIZE 6 * 64 * 1024
#include "stack-check-prologue.h"

/* { dg-final { scan-assembler-times {stp*t*r*.d\t\$r0,\$r3,0} 1 } } */

/* SIZE is more than 4x guard-size and no remainder, 1 probe expected in a loop
   and no residual probe.  */
