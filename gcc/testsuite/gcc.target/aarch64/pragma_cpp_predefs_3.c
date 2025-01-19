/* { dg-do compile } */
/* { dg-options "-O2" } */

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a")

#ifdef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERM is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SM4
#error "__ARM_FEATURE_SVE2_SM4 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2p1
#error "__ARM_FEATURE_SVE2p1 is defined but should not be!"
#endif

#pragma GCC push_options
#pragma GCC target ("arch=armv8.2-a+sve")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifdef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERM is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SM4
#error "__ARM_FEATURE_SVE2_SM4 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2p1
#error "__ARM_FEATURE_SVE2p1 is defined but should not be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.5-a+sve2")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifdef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERM is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SM4
#error "__ARM_FEATURE_SVE2_SM4 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2p1
#error "__ARM_FEATURE_SVE2p1 is defined but should not be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.5-a+sve2-aes")

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

#ifdef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERM is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SM4
#error "__ARM_FEATURE_SVE2_SM4 is defined but should not be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.5-a+sve2-bitperm")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifdef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is defined but should not be!"
#endif

#ifndef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERM is not defined but should be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SM4
#error "__ARM_FEATURE_SVE2_SM4 is defined but should not be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.5-a+sve2-sha3")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifdef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERM is defined but should not be!"
#endif

#ifndef __ARM_FEATURE_SHA3
#error "__ARM_FEATURE_SHA3 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is not defined but should be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SM4
#error "__ARM_FEATURE_SVE2_SM4 is defined but should not be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.5-a+sve2-sm4")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifdef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERM is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is defined but should not be!"
#endif

#ifndef __ARM_FEATURE_SM4
#error "__ARM_FEATURE_SM4 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_SM4
#error "__ARM_FEATURE_SVE2_SM4 is not defined but should be!"
#endif

#pragma GCC pop_options

/* Make sure that general-regs-only works too.  */
#pragma GCC push_options
#pragma GCC target ("arch=armv8.5-a+sve2-aes+sve2-bitperm+sve2-sha3+sve2-sm4")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERM is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_SM4
#error "__ARM_FEATURE_SVE2_SM4 is not defined but should be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv9-a+sve2p1")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifdef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERM is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SM4
#error "__ARM_FEATURE_SVE2_SM4 is defined but should not be!"
#endif

#ifndef __ARM_FEATURE_SVE2p1
#error "__ARM_FEATURE_SVE2p1 is not defined but should be!"
#endif

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv9-a+sve2-aes+sve2-bitperm+sve2-sha3+sve2-sm4+sve2p1")

#ifndef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERM is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2_SM4
#error "__ARM_FEATURE_SVE2_SM4 is not defined but should be!"
#endif

#ifndef __ARM_FEATURE_SVE2p1
#error "__ARM_FEATURE_SVE2p1 is not defined but should be!"
#endif

#pragma GCC push_options
#pragma GCC target ("general-regs-only")

#ifdef __ARM_FEATURE_SVE
#error "__ARM_FEATURE_SVE is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2
#error "__ARM_FEATURE_SVE2 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_AES
#error "__ARM_FEATURE_SVE2_AES is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_BITPERM
#error "__ARM_FEATURE_SVE2_BITPERM is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SHA3
#error "__ARM_FEATURE_SVE2_SHA3 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2_SM4
#error "__ARM_FEATURE_SVE2_SM4 is defined but should not be!"
#endif

#ifdef __ARM_FEATURE_SVE2p1
#error "__ARM_FEATURE_SVE2p1 is defined but should not be!"
#endif

#pragma GCC pop_options

#pragma GCC pop_options

#pragma GCC pop_options

int
foo (int a)
{
  return a;
}
