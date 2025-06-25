/* { dg-do compile { target { aarch64*-*-linux* && { ! aarch64_gas_has_build_attributes } } } } */
/* { dg-options "-mbranch-protection=bti -dA" } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler-not "\.aeabi_subsection" } } */
/* { dg-final { scan-assembler-not "\.aeabi_attribute" } } */
/* { dg-final { scan-assembler "\.section\t\.note\.gnu\.property" } } */
/* { dg-final { scan-assembler "\.word\t0x1\t\/\/ GNU_PROPERTY_AARCH64_FEATURE_1_AND \\(BTI\\)" } } */