/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-skip-if "" { *-*-* } { "-fstack-check" } { "" } } */

#define SIZE 100
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-times {st\.d\t\$r0,\$r3,104} 1 } } */

/* Alloca is less than guard-size, 1 probe at the top of the new allocation.  */
