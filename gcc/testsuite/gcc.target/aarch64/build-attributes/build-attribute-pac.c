/* { dg-do compile { target { aarch64*-*-linux* && { aarch64_gas_has_build_attributes } } } } */
/* { dg-options "-mbranch-protection=pac-ret -dA" } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler "\.aeabi_subsection aeabi_feature_and_bits, optional, ULEB128" } } */
/* { dg-final { scan-assembler "\.aeabi_attribute Tag_Feature_PAC, 1\t\/\/ Tag_Feature_PAC: true" } } */
/* { dg-final { scan-assembler-not "\.section\t\.note\.gnu\.property" } } */
