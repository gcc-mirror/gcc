/* { dg-do assemble { target { { aarch64_asm_sve-sm4_ok && aarch64_asm_sve-sha3_ok } && { aarch64_asm_sve-aes_ok && aarch64_asm_sve-bitperm_ok } } } } */
/* { dg-do compile { target { ! { { aarch64_asm_sve-sm4_ok && aarch64_asm_sve-sha3_ok } && { aarch64_asm_sve-aes_ok && aarch64_asm_sve-bitperm_ok } } } } } */
/* { dg-options "-O2" } */

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a")

#ifdef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_AES
#error "__ARM_FEATURE_AES is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_BitPerm
#error "__ARM_FEATURE_SVE2_BitPerm is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SM4
#error "__ARM_FEATURE_SM4 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SM4
#error "__ARM_FEATURE_SVE2_SM4 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SHA3
#error "__ARM_FEATURE_SHA3 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is defined but should not be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a+sve-bitperm")

#ifdef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_BitPerm
#error "__ARM_FEATURE_SVE2_BitPerm is defined but should not be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a+sve2-bitperm")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERM is not defined but should be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a+sve2+sve-bitperm")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERMis not defined but should be!"
#endif

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a+sve-sha3")

#ifdef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is defined but should not be!"
#endif

#ifndef __ARM_FEATURE_SHA3
#error "__ARM_FEATURE_SHA3 is not defined but should be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is defined but should not be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a+sve2-sha3")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SHA3
#error "__ARM_FEATURE_SHA3 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is not defined but should be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a+sve2+sve-sha3")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SHA3
#error "__ARM_FEATURE_SHA3 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is not defined but should be!"
#endif
#pragma GCC push_options
#pragma GCC target ("arch=armv8-a+sve-aes")

#ifdef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is defined but should not be!"
#endif

#ifndef __ARM_FEATURE_AES
#error "__ARM_FEATURE_AES is not defined but should be!"
#endif

#ifdef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is defined but should not be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a+sve2-aes")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_AES
#error "__ARM_FEATURE_AES is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is not defined but should be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a+sve2+sve-aes")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_AES
#error "__ARM_FEATURE_AES is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is not defined but should be!"
#endif
#pragma GCC pop_options

int
foo (int a)
{
  return a;
}
