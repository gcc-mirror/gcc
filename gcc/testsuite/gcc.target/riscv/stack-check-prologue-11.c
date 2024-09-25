/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection -fno-stack-protector" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE (6 * 4 * 1024) + (1 * 2 * 1024)
#include "stack-check-prologue.h"

/* { dg-final { scan-assembler-times {sd\tzero,1024\(sp\)} 1 } } */

/* SIZE is more than 4x guard-size and remainder larger than guard-size - 1Kb,
   1 probe expected in a loop and 1 residual probe.  */
