/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 127 * 3 * 1024
#include "stack-check-alloca.h"

/* { dg-final { scan-assembler-times {sd\tzero,1024\(sp\)} 1 } } */

/* Large alloca of a constant amount which is a multiple of a guard-size,
   no residiual.  Loop expected with one 1Kb probe offset and no residual probe
   because residual is at compile time known to be zero.  */
