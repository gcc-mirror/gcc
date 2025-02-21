/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 2.5 * 1024
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-times {sd\tzero,1024\(sp\)} 1 } } */

/* Alloca is more than 1kB, but less than guard-size, 1 probe expected at 1kB
   offset.  */
