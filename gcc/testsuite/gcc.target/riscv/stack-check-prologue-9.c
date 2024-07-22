/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection -fno-stack-protector" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 6 * 4 * 1024
#include "stack-check-prologue.h"

/* { dg-final { scan-assembler-times {sd\tzero,1024\(sp\)} 1 } } */

/* SIZE is more than 4x guard-size and no remainder, 1 probe expected in a loop
   and no residual probe.  */
