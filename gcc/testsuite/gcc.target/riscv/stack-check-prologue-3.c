/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 3 * 1024
#include "stack-check-prologue.h"

/* { dg-final { scan-assembler-times "sd\tzero," 1 } } */

/* SIZE is exactly guard-size - 1Kb, boundary condition so 1 probe expected.
*/
