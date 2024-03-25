/* { dg-options "-c -fdump-go-spec=godump-2.out" } */
/* { dg-do compile { target bitint } } */
/* { dg-skip-if "not supported for target" { ! "alpha*-*-* s390*-*-* i?86-*-* x86_64-*-*" } } */
/* { dg-skip-if "not supported for target" { ! lp64 } } */

_BitInt(32) b32_v;
/* { dg-final { scan-file godump-2.out "(?n)^var _b32_v int32$" } } */

_BitInt(64) b64_v;
/* { dg-final { scan-file godump-2.out "(?n)^var _b64_v int64$" } } */

unsigned _BitInt(32) b32u_v;
/* { dg-final { scan-file godump-2.out "(?n)^var _b32u_v uint32$" } } */

_BitInt(33) b33_v;
/* { dg-final { scan-file godump-2.out "(?n)^// var _b33_v INVALID-bitint-33$" } } */

/* { dg-final { remove-build-file "godump-2.out" } } */
