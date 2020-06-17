/* Test for CDE #pragma target macros.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_v8m_main_cde_ok } */
/* { dg-add-options arm_v8m_main_cde } */

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main")
#ifdef __ARM_FEATURE_CDE
#error "__ARM_FEATURE_CDE is defined but should not be"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp0")
#ifndef __ARM_FEATURE_CDE
#error "__ARM_FEATURE_CDE is not defined but should be"
#endif
#if __ARM_FEATURE_CDE_COPROC != 0x1
#error "__ARM_FEATURE_CDE_COPROC is not defined as configured"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp1")
#ifndef __ARM_FEATURE_CDE
#error "__ARM_FEATURE_CDE is not defined but should be"
#endif
#if __ARM_FEATURE_CDE_COPROC != 0x2
#error "__ARM_FEATURE_CDE_COPROC is not defined as configured"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp2")
#ifndef __ARM_FEATURE_CDE
#error "__ARM_FEATURE_CDE is not defined but should be"
#endif
#if __ARM_FEATURE_CDE_COPROC != 0x4
#error "__ARM_FEATURE_CDE_COPROC is not defined as configured"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp3")
#ifndef __ARM_FEATURE_CDE
#error "__ARM_FEATURE_CDE is not defined but should be"
#endif
#if __ARM_FEATURE_CDE_COPROC != 0x8
#error "__ARM_FEATURE_CDE_COPROC is not defined as configured"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp4")
#ifndef __ARM_FEATURE_CDE
#error "__ARM_FEATURE_CDE is not defined but should be"
#endif
#if __ARM_FEATURE_CDE_COPROC != 0x10
#error "__ARM_FEATURE_CDE_COPROC is not defined as configured"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp5")
#ifndef __ARM_FEATURE_CDE
#error "__ARM_FEATURE_CDE is not defined but should be"
#endif
#if __ARM_FEATURE_CDE_COPROC != 0x20
#error "__ARM_FEATURE_CDE_COPROC is not defined as configured"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp6")
#ifndef __ARM_FEATURE_CDE
#error "__ARM_FEATURE_CDE is not defined but should be"
#endif
#if __ARM_FEATURE_CDE_COPROC != 0x40
#error "__ARM_FEATURE_CDE_COPROC is not defined as configured"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp7")
#ifndef __ARM_FEATURE_CDE
#error "__ARM_FEATURE_CDE is not defined but should be"
#endif
#if __ARM_FEATURE_CDE_COPROC != 0x80
#error "__ARM_FEATURE_CDE_COPROC is not defined as configured"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp0+cdecp1")
#if __ARM_FEATURE_CDE_COPROC != 0x3
#error "__ARM_FEATURE_CDE_COPROC is not defined as configured"
#endif
#pragma GCC pop_options
