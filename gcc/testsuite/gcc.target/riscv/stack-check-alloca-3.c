/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 100
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-times {sd\tzero,8\(sp\)} 1 } } */

/* Alloca is less than 1kB, 1 probe expected at word offset.  */
