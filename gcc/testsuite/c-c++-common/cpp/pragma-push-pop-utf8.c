/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic" { target c } } */
/* { dg-options "-std=c++11 -pedantic" { target c++ } } */
/* { dg-additional-options "-Wall" } */

/* PR preprocessor/109704 */

/* Verify basic operations for different extended identifiers...  */

/* ...dollar sign.  */
#define $x 1
#pragma push_macro("$x")
#undef $x
#define $x 0
#pragma pop_macro("$x")
#if !$x
#error $x
#endif
#define $x 1
_Pragma("push_macro(\"$x\")")
#undef $x
#define $x 0
_Pragma("pop_macro(\"$x\")")
#if !$x
#error $x
#endif
#define x$ 1
#pragma push_macro("x$")
#undef x$
#define x$ 0
#pragma pop_macro("x$")
#if !x$
#error x$
#endif
#define x$ 1
_Pragma("push_macro(\"x$\")")
#undef x$
#define x$ 0
_Pragma("pop_macro(\"x$\")")
#if !x$
#error x$
#endif

/* ...UCN.  */
#define \u03B1x 1
#pragma push_macro("\u03B1x")
#undef \u03B1x
#define \u03B1x 0
#pragma pop_macro("\u03B1x")
#if !\u03B1x
#error \u03B1x
#endif
#define \u03B1x 1
_Pragma("push_macro(\"\\u03B1x\")")
#undef \u03B1x
#define \u03B1x 0
_Pragma("pop_macro(\"\\u03B1x\")")
#if !\u03B1x
#error \u03B1x
#endif
#define x\u03B1 1
#pragma push_macro("x\u03B1")
#undef x\u03B1
#define x\u03B1 0
#pragma pop_macro("x\u03B1")
#if !x\u03B1
#error x\u03B1
#endif
#define x\u03B1 1
_Pragma("push_macro(\"x\\u03B1\")")
#undef x\u03B1
#define x\u03B1 0
_Pragma("pop_macro(\"x\\u03B1\")")
#if !x\u03B1
#error x\u03B1
#endif

/* ...UTF-8.  */
#define πx 1
#pragma push_macro("πx")
#undef πx
#define πx 0
#pragma pop_macro("πx")
#if !πx
#error πx
#endif
#define πx 1
_Pragma("push_macro(\"πx\")")
#undef πx
#define πx 0
_Pragma("pop_macro(\"πx\")")
#if !πx
#error πx
#endif
#define xπ 1
#pragma push_macro("xπ")
#undef xπ
#define xπ 0
#pragma pop_macro("xπ")
#if !xπ
#error xπ
#endif
#define xπ 1
_Pragma("push_macro(\"xπ\")")
#undef xπ
#define xπ 0
_Pragma("pop_macro(\"xπ\")")
#if !xπ
#error xπ
#endif

/* Verify UCN and UTF-8 can be intermixed.  */
#define ħ_0 1
#pragma push_macro("ħ_0")
#undef ħ_0
#define ħ_0 0
#if ħ_0
#error ħ_0 ħ_0 \U00000127_0
#endif
#pragma pop_macro("\U00000127_0")
#if !ħ_0
#error ħ_0 ħ_0 \U00000127_0
#endif
#define ħ_1 1
#pragma push_macro("\U00000127_1")
#undef ħ_1
#define ħ_1 0
#if ħ_1
#error ħ_1 \U00000127_1 ħ_1
#endif
#pragma pop_macro("ħ_1")
#if !ħ_1
#error ħ_1 \U00000127_1 ħ_1
#endif
#define ħ_2 1
#pragma push_macro("\U00000127_2")
#undef ħ_2
#define ħ_2 0
#if ħ_2
#error ħ_2 \U00000127_2 \U00000127_2
#endif
#pragma pop_macro("\U00000127_2")
#if !ħ_2
#error ħ_2 \U00000127_2 \U00000127_2
#endif
#define \U00000127_3 1
#pragma push_macro("ħ_3")
#undef \U00000127_3
#define \U00000127_3 0
#if \U00000127_3
#error \U00000127_3 ħ_3 ħ_3
#endif
#pragma pop_macro("ħ_3")
#if !\U00000127_3
#error \U00000127_3 ħ_3 ħ_3
#endif
#define \U00000127_4 1
#pragma push_macro("ħ_4")
#undef \U00000127_4
#define \U00000127_4 0
#if \U00000127_4
#error \U00000127_4 ħ_4 \U00000127_4
#endif
#pragma pop_macro("\U00000127_4")
#if !\U00000127_4
#error \U00000127_4 ħ_4 \U00000127_4
#endif
#define \U00000127_5 1
#pragma push_macro("\U00000127_5")
#undef \U00000127_5
#define \U00000127_5 0
#if \U00000127_5
#error \U00000127_5 \U00000127_5 ħ_5
#endif
#pragma pop_macro("ħ_5")
#if !\U00000127_5
#error \U00000127_5 \U00000127_5 ħ_5
#endif

/* Verify invalid input produces no diagnostics.  */
#pragma push_macro("") /* { dg-bogus "." } */
#pragma push_macro("\u") /* { dg-bogus "." } */
#pragma push_macro("\u0000") /* { dg-bogus "." } */
#pragma push_macro("not a single identifier") /* { dg-bogus "." } */
#pragma push_macro("invalid╬character") /* { dg-bogus "." } */
#pragma push_macro("\u0300invalid_start") /* { dg-bogus "." } */
#pragma push_macro("#include <cstdlib>") /* { dg-bogus "." } */

/* Verify end-of-line diagnostics for valid and invalid input.  */
#pragma push_macro("ö") oops /* { dg-warning "extra tokens" } */
#pragma push_macro("") oops /* { dg-warning "extra tokens" } */
#pragma push_macro("\u") oops /* { dg-warning "extra tokens" } */
#pragma push_macro("\u0000") oops /* { dg-warning "extra tokens" } */
#pragma push_macro("not a single identifier") oops /* { dg-warning "extra tokens" } */
#pragma push_macro("invalid╬character") oops /* { dg-warning "extra tokens" } */
#pragma push_macro("\u0300invalid_start") oops /* { dg-warning "extra tokens" } */
#pragma push_macro("#include <cstdlib>") oops /* { dg-warning "extra tokens" } */

/* Verify expected diagnostics.  */
#pragma push_macro() /* { dg-error {invalid '#pragma push_macro'} } */
#pragma pop_macro() /* { dg-error {invalid '#pragma pop_macro'} } */
_Pragma("push_macro(0)") /* { dg-error {invalid '#pragma push_macro'} } */
_Pragma("pop_macro(\"oops\"") /* { dg-error {invalid '#pragma pop_macro'} } */
