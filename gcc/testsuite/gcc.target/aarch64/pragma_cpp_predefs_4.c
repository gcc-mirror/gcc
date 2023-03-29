#pragma GCC target "+nothing+dotprod"
#ifndef __ARM_FEATURE_FMA
#error Foo
#endif

#pragma GCC target "+nothing+aes"
#ifndef __ARM_FEATURE_FMA
#error Foo
#endif

#pragma GCC target "+nothing+sha2"
#ifndef __ARM_FEATURE_FMA
#error Foo
#endif

#pragma GCC target "+nothing+sha3"
#ifndef __ARM_FEATURE_FMA
#error Foo
#endif

#pragma GCC target "+nothing+sm4"
#ifndef __ARM_FEATURE_FMA
#error Foo
#endif

#pragma GCC target "+crypto+noaes"
#ifdef __ARM_FEATURE_CRYPTO
#error Foo
#endif

#pragma GCC target "+crypto+nosha2"
#ifdef __ARM_FEATURE_CRYPTO
#error Foo
#endif

#pragma GCC target "+nothing+sve2-sha3"
#ifndef __ARM_FEATURE_SHA2
#error Foo
#endif

#pragma GCC target "+sve2-sha3+nosha2"
#ifdef __ARM_FEATURE_SHA3
#error Foo
#endif
#ifdef __ARM_FEATURE_SVE2_SHA3
#error Foo
#endif
