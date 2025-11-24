/* { dg-do compile { target { aarch64*-*-linux* && { ! aarch64_gas_has_build_attributes } } } } */

#if defined(__ARM_BUILDATTR64_FV)
#error "Support for build attributes should not be enabled in this toolchain."
#endif
