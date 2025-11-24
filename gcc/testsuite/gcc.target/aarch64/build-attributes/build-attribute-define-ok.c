/* { dg-do compile { target { aarch64*-*-linux* && { aarch64_gas_has_build_attributes } } } } */

#if ! defined(__ARM_BUILDATTR64_FV)
#error "Support for build attributes should be enabled in this toolchain."
#elif __ARM_BUILDATTR64_FV != 'A'
#error "The current build attributes version does not match the expected one."
#endif
