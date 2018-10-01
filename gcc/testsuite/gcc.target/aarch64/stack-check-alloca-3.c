/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 100
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-times {str\s+xzr, \[sp, 8\]} 1 } } */

/* Alloca is less than 1kB, 1 probe expected at word offset.  */
