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
#ifdef __ARM_FEATURE_SME_F8F16
#error Foo
#endif
#ifdef __ARM_FEATURE_SME_F8F32
#error Foo
#endif
#ifdef __ARM_FEATURE_SME_B16B16
#error Foo
#endif
#ifdef __ARM_FEATURE_SME_F16F16
#error Foo
#endif
#ifdef __ARM_FEATURE_SME_F64F64
#error Foo
#endif

#pragma GCC target "+nothing+sme-i16i64"
#ifndef __ARM_FEATURE_SME_I16I64
#error Foo
#endif
#ifndef __ARM_FEATURE_SME
#error Foo
#endif
#ifdef __ARM_FEATURE_SME2
#error Foo
#endif

#pragma GCC target "+nothing+sme-b16b16"
#ifndef __ARM_FEATURE_SME_B16B16
#error Foo
#endif
#ifndef __ARM_FEATURE_SME
#error Foo
#endif
#ifndef __ARM_FEATURE_SME2
#error Foo
#endif
#ifndef __ARM_FEATURE_SVE_B16B16
#error Foo
#endif

#pragma GCC target "+nothing+sve-bfscale"
#ifndef __ARM_FEATURE_SVE_BFSCALE
#error "__ARM_FEATURE_SVE_BFSCALE should be defined but isn't"
#endif

#pragma GCC target "+nothing+sve2+sme-f8f16"
#ifndef __ARM_FEATURE_SME_F8F16
#error Foo
#endif
#ifndef __ARM_FEATURE_SME
#error Foo
#endif
#ifndef __ARM_FEATURE_SME2
#error Foo
#endif
#ifndef __ARM_FEATURE_FP8
#error Foo
#endif

#pragma GCC target "+nothing+sve2+sme-f8f32"
#ifndef __ARM_FEATURE_SME_F8F32
#error Foo
#endif
#ifndef __ARM_FEATURE_SME
#error Foo
#endif
#ifndef __ARM_FEATURE_SME2
#error Foo
#endif
#ifndef __ARM_FEATURE_FP8
#error Foo
#endif

#pragma GCC target "+nothing+sme-f16f16"
#ifndef __ARM_FEATURE_SME_F16F16
#error Foo
#endif
#ifndef __ARM_FEATURE_SME
#error Foo
#endif
#ifndef __ARM_FEATURE_SME2
#error Foo
#endif

#pragma GCC target "+nothing+sme-f64f64"
#ifndef __ARM_FEATURE_SME_F64F64
#error Foo
#endif
#ifndef __ARM_FEATURE_SME
#error Foo
#endif
#ifdef __ARM_FEATURE_SME2
#error Foo
#endif

#pragma GCC target "+nothing+sve-b16b16"
#ifdef __ARM_FEATURE_SVE_B16B16
#error Foo
#endif
#ifdef __ARM_FEATURE_SVE
#error Foo
#endif
#ifdef __ARM_FEATURE_SME
#error Foo
#endif

#pragma GCC target "+nothing+sve-b16b16+sve"
#ifdef __ARM_FEATURE_SVE_B16B16
#error Foo
#endif
#ifndef __ARM_FEATURE_SVE
#error Foo
#endif
#ifdef __ARM_FEATURE_SME
#error Foo
#endif

#pragma GCC target "+nothing+sve-b16b16+sve2"
#ifndef __ARM_FEATURE_SVE_B16B16
#error Foo
#endif
#ifndef __ARM_FEATURE_SVE
#error Foo
#endif
#ifdef __ARM_FEATURE_SME
#error Foo
#endif

#pragma GCC target "+nothing+sve-b16b16+sme2"
#ifndef __ARM_FEATURE_SVE_B16B16
#error Foo
#endif
#ifndef __ARM_FEATURE_SME
#error Foo
#endif

#pragma GCC target "+nothing+sme2p1"
#ifndef __ARM_FEATURE_SME
#error Foo
#endif
#ifndef __ARM_FEATURE_SME2
#error Foo
#endif
#ifndef __ARM_FEATURE_SME2p1
#error Foo
#endif

#pragma GCC target "+nothing+sve2+sme2p2"
#ifndef __ARM_FEATURE_SME
#error Foo
#endif
#ifndef __ARM_FEATURE_SME2
#error Foo
#endif
#ifndef __ARM_FEATURE_SME2p1
#error Foo
#endif
#ifndef __ARM_FEATURE_SME2p2
#error Foo
#endif
#pragma GCC target "+nothing+sve2+sme-mop4"
#ifndef __ARM_FEATURE_SME_MOP4
#error "__ARM_FEATURE_SME_MOP4 not defined"
#endif
#ifndef __ARM_FEATURE_SME2
#error "__ARM_FEATURE_SME2 not defined"
#endif

#pragma GCC target "branch-protection=standard"
#ifndef __ARM_FEATURE_BTI_DEFAULT
#error Foo
#endif
#if __ARM_FEATURE_PAC_DEFAULT != 1
#error Foo
#endif
#ifndef __ARM_FEATURE_GCS_DEFAULT
#error Foo
#endif

#pragma GCC target ("branch-protection=none")
#ifdef __ARM_FEATURE_BTI_DEFAULT
#error Foo
#endif
#ifdef __ARM_FEATURE_PAC_DEFAULT
#error Foo
#endif
#ifdef __ARM_FEATURE_GCS_DEFAULT
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
#ifdef __ARM_FEATURE_GCS_DEFAULT
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

#pragma GCC target "branch-protection=gcs"
#ifdef __ARM_FEATURE_BTI_DEFAULT
#error Foo
#endif
#ifdef __ARM_FEATURE_PAC_DEFAULT
#error Foo
#endif
#ifndef __ARM_FEATURE_GCS_DEFAULT
#error Foo
#endif

#pragma GCC target "arch=armv8.8-a+gcs"
#ifndef __ARM_FEATURE_GCS
#error Foo
#endif

#pragma GCC target "arch=armv8.8-a+nogcs"
#ifdef __ARM_FEATURE_GCS
#error Foo
#endif

#pragma GCC target "arch=armv8.8-a"
#ifdef __ARM_FEATURE_GCS
#error Foo
#endif

#pragma GCC target "arch=armv9-a"
#ifdef __ARM_FEATURE_FP8
#error Foo
#endif

#pragma GCC target "arch=armv9-a+fp8"
#ifndef __ARM_FEATURE_FP8
#error Foo
#endif

#pragma GCC target "arch=armv9-a+fp8fma"
#ifndef __ARM_FEATURE_FP8
#error Foo
#endif
#ifdef __ARM_FEATURE_FP8DOT4
#error Foo
#endif
#ifdef __ARM_FEATURE_FP8DOT2
#error Foo
#endif
#ifndef __ARM_FEATURE_FP8FMA
#error Foo
#endif

#pragma GCC target "arch=armv9-a+fp8dot4"
#ifndef __ARM_FEATURE_FP8
#error Foo
#endif
#ifdef __ARM_FEATURE_FP8FMA
#error Foo
#endif
#ifndef __ARM_FEATURE_FP8DOT4
#error Foo
#endif
#ifdef __ARM_FEATURE_FP8DOT2
#error Foo
#endif

#pragma GCC target "arch=armv9-a+fp8dot2"
#ifndef __ARM_FEATURE_FP8
#error Foo
#endif
#ifdef __ARM_FEATURE_FP8FMA
#error Foo
#endif
#ifdef __ARM_FEATURE_FP8DOT4
#error Foo
#endif
#ifndef __ARM_FEATURE_FP8DOT2
#error Foo
#endif

#pragma GCC target "arch=armv9.4-a"
#ifdef __ARM_FEATURE_FAMINMAX
#error Foo
#endif

#pragma GCC target "arch=armv9.5-a"
#ifndef __ARM_FEATURE_FAMINMAX
#error Foo
#endif

#pragma GCC target "arch=armv8-a+faminmax"
#ifndef __ARM_FEATURE_FAMINMAX
#error Foo
#endif

#pragma GCC target "arch=armv8-a+sve2+ssve-fexpa"
#ifndef __ARM_FEATURE_SSVE_FEXPA
#error Foo
#endif

#pragma GCC target "arch=armv8.8-a"
#ifdef __ARM_FEATURE_CSSC
#error Foo
#endif

#pragma GCC target "arch=armv8.9-a"
#ifndef __ARM_FEATURE_CSSC
#error Foo
#endif

#pragma GCC target "arch=armv9.3-a"
#ifdef __ARM_FEATURE_CSSC
#error Foo
#endif

#pragma GCC target "arch=armv9.4-a"
#ifndef __ARM_FEATURE_CSSC
#error Foo
#endif

#pragma GCC target "arch=armv8-a+cssc"
#ifndef __ARM_FEATURE_CSSC
#error Foo
#endif

#pragma GCC target "+nothing+cssc"
#ifndef __ARM_FEATURE_CSSC
#error Foo
#endif

#pragma GCC target "arch=armv8.9-a+nocssc"
#ifdef __ARM_FEATURE_CSSC
#error Foo
#endif
