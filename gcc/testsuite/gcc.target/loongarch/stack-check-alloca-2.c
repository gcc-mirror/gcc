/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-skip-if "" { *-*-* } { "-fstack-check" } { "" } } */

#define SIZE 0
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-not {stp*t*r*\.d\t\$r0,\$r3,4088} } } */

/* Alloca of 0 should emit no probes, boundary condition.  */
