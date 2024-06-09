/* Test the __arm_[r,w]sr ACLE intrinsics family.  */
/* Ensure that illegal behavior is rejected by the compiler.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a" } */

#include <arm_acle.h>

/* Ensure that read/write-only register attributes are respected by the compiler.  */
void
test_rwsr_read_write_only ()
{
  /* Attempt to write to read-only registers.  */
  long long a = __arm_rsr64 ("aidr_el1");  /* Read ok.  */
  __arm_wsr64 ("aidr_el1", a); /* { dg-error {invalid system register name 'aidr_el1'} } */

  /* Attempt to read from write-only registers.  */
  __arm_wsr64 ("icc_asgi1r_el1", a);            /* Write ok.  */
  long long b = __arm_rsr64 ("icc_asgi1r_el1"); /* { dg-error {invalid system register name 'icc_asgi1r_el1'} } */
}

/* Ensure that empty strings are rejected.  */
void
test_empty_string ()
{
  long long c = __arm_rsr64 (""); /* { dg-error "invalid system register name ''" } */
  __arm_wsr64 ("", c); /* { dg-error "invalid system register name ''" } */
}
