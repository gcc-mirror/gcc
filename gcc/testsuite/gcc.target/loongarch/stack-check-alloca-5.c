/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-skip-if "" { *-*-* } { "-fstack-check" } { "" } } */

#define SIZE 65 * 1024
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-times {stp*t*r*\.d\t\$r0,\$r\d{1,2},-8} 1 } } */
/* { dg-final { scan-assembler-times {stp*t*r*\.d\t\$r0,\$r3,1016} 1 } } */

/* Alloca is more than one guard-page. 2 probes expected.  */
