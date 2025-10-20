/* { dg-do compile } */
/* { dg-options "-menable-sysreg-checking -march=armv8-a" } */
/* Ensure the system registers are rejected by compiler when guarding is
   enabled through "-menable-sysreg-checking" command line flag and proper
   feature flags are not passed.  */

#include <arm_acle.h>

uint64_t
foo (uint64_t a)
{
  __arm_wsr64 ("zcr_el1", a); /* { dg-error "invalid system register name 'zcr_el1'" } */
  return __arm_rsr64 ("smcr_el1"); /* { dg-error "invalid system register name 'smcr_el1'" } */
}
