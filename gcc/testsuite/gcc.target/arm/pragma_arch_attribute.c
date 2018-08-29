/* Test for #pragma target macros.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-add-options arm_arch_v8a } */

#include <arm_acle.h>

#ifdef __ARM_FEATURE_CRC32
# error "__ARM_FEATURE_CRC32 is already defined."
#endif

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a+crc")
#ifndef __ARM_FEATURE_CRC32
# error "__ARM_FEATURE_CRC32 is not defined in push 1."
#endif
#pragma GCC pop_options

#ifdef __ARM_FEATURE_CRC32
# error "__ARM_FEATURE_CRC32 is defined after pop 1."
#endif

#pragma GCC push_options
#pragma GCC target ("+crc")
#ifndef __ARM_FEATURE_CRC32
# error "__ARM_FEATURE_CRC32 is not defined in push 2."
#endif
#pragma GCC pop_options

#ifdef __ARM_FEATURE_CRC32
# error "__ARM_FEATURE_CRC32 is defined after pop 2."
#endif

__attribute__((target("+crc")))
void test_crc_unknown_ok_attr_1 ()
{
	__crc32b (0, 0);
}

#ifdef __ARM_FEATURE_CRC32
# error "__ARM_FEATURE_CRC32 is defined after attribute set 1."
#endif

__attribute__((target("arch=armv8-a+crc")))
void test_crc_unknown_ok_attr_2 ()
{
	__crc32b (0, 0);
}

#ifdef __ARM_FEATURE_CRC32
# error "__ARM_FEATURE_CRC32 is defined after attribute set 2."
#endif

#pragma GCC reset_options