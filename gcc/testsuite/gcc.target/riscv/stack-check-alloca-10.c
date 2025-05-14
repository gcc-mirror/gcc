/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 127.5 * 3 * 1024
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-times {sd\tzero,1024\(sp\)} 2 } } */

/* Large alloca of an amount which isn't a multiple of a guard-size, and
   residiual is more than 1kB.  Loop expected with one 1Kb probe offset and
   one residual probe at offset 1kB.  */
