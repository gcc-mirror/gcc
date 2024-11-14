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

#pragma GCC target "branch-protection=standard"
#ifndef __ARM_FEATURE_BTI_DEFAULT
#error Foo
#endif
#if __ARM_FEATURE_PAC_DEFAULT != 1
#error Foo
#endif

#pragma GCC target ("branch-protection=none")
#ifdef __ARM_FEATURE_BTI_DEFAULT
#error Foo
#endif
#ifdef __ARM_FEATURE_PAC_DEFAULT
#error Foo
#endif

#pragma GCC push_options
#pragma GCC target "branch-protection=bti+pac-ret"
#ifndef __ARM_FEATURE_BTI_DEFAULT
#error Foo
#endif
#pragma GCC pop_options
#ifdef __ARM_FEATURE_BTI_DEFAULT
#error Foo
#endif

#pragma GCC target "branch-protection=bti"
#ifndef __ARM_FEATURE_BTI_DEFAULT
#error Foo
#endif
#ifdef __ARM_FEATURE_PAC_DEFAULT
#error Foo
#endif

#pragma GCC target "branch-protection=pac-ret"
#ifdef __ARM_FEATURE_BTI_DEFAULT
#error Foo
#endif
#if __ARM_FEATURE_PAC_DEFAULT != 1
#error Foo
#endif

#pragma GCC target "branch-protection=pac-ret+leaf+b-key"
#ifdef __ARM_FEATURE_BTI_DEFAULT
#error Foo
#endif
#if __ARM_FEATURE_PAC_DEFAULT != 6
#error Foo
#endif
