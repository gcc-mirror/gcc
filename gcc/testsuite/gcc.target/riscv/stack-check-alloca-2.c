/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 0
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-not {sd\tzero,} } } */

/* Alloca of 0 should emit no probes, boundary condition.  */
