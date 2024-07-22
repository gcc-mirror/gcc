/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 7 * 1024
#include "stack-check-prologue.h"

/* { dg-final { scan-assembler-times {sd\tzero,1024\(sp\)} 2 } } */

/* SIZE is more than 1x guard-size and remainder equal than guard-size - 1Kb,
   2 probe expected, unrolled, no loop.  */
