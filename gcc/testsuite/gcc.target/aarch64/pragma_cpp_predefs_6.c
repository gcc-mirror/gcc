#pragma GCC target "arch=armv9.5-a+sme"
#ifdef __ARM_FEATURE_SME_TMOP
#error Foo
#endif

#pragma GCC target "arch=armv9-a+sme-tmop"
#ifndef __ARM_FEATURE_SME_TMOP
#error Foo
#endif

#ifndef __ARM_FEATURE_SME
#error Foo
#endif

#ifndef __ARM_FEATURE_SME2
#error Foo
#endif
