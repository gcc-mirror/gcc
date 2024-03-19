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

#pragma GCC target "+sme"
#ifndef __ARM_FEATURE_SME
#error Foo
#endif

#pragma GCC target "+sme+nofp"
#ifdef __ARM_FEATURE_SME
#error Foo
#endif

#pragma GCC target "+sme+nosimd"
#ifdef __ARM_FEATURE_SME
#error Foo
#endif

#pragma GCC target "+sme+nobf16"
#ifdef __ARM_FEATURE_SME
#error Foo
#endif

#pragma GCC target "+nothing+sme"
#ifdef __ARM_FEATURE_SME_I16I64
#error Foo
#endif
#ifdef __ARM_FEATURE_SME_F64F64
#error Foo
#endif

#pragma GCC target "+sme-i16i64"
#ifndef __ARM_FEATURE_SME_I16I64
#error Foo
#endif

#pragma GCC target "+sme-f64f64"
#ifndef __ARM_FEATURE_SME_F64F64
#error Foo
#endif
