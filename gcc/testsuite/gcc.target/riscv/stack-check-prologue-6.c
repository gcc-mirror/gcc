/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection -fno-stack-protector" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 5 * 1024
#include "stack-check-prologue.h"

/* { dg-final { scan-assembler-times {sd\tzero,1024\(sp\)} 1 } } */

/* SIZE is more than guard-size - 1Kb and remainder is equal to 1kB,
   1 probe expected.  */
