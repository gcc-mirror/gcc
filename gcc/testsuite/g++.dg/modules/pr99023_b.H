# 0 "pr99023_b.H"
// PR c++/99023, ICE
// { dg-additional-options {-std=c++17 -Wno-pedantic -fmodule-header -fpreprocessed -fdirectives-only --param ggc-min-expand=0} }
// { dg-module-cmi {,/pr99023_b.H} }
# 0 "<built-in>"
#define __STDC__ 1
#define __cplusplus 201703L
#define __STDC_UTF_16__ 1
#define __STDC_UTF_32__ 1
#define __STDC_HOSTED__ 1
#define __GNUC__ 11
#define __GNUC_MINOR__ 0
#define __GNUC_PATCHLEVEL__ 0
#define __VERSION__ "11.0.0 20210217 (experimental)"
#define __ATOMIC_RELAXED 0
#define __ATOMIC_SEQ_CST 5
#define __ATOMIC_ACQUIRE 2
#define __ATOMIC_RELEASE 3
#define __ATOMIC_ACQ_REL 4
#define __ATOMIC_CONSUME 1
#define __FINITE_MATH_ONLY__ 0
#define _LP64 1
#define __LP64__ 1
#define __SIZEOF_INT__ 4
#define __SIZEOF_LONG__ 8
#define __SIZEOF_LONG_LONG__ 8
#define __SIZEOF_SHORT__ 2
#define __SIZEOF_FLOAT__ 4
#define __SIZEOF_DOUBLE__ 8
#define __SIZEOF_LONG_DOUBLE__ 16
#define __SIZEOF_SIZE_T__ 8
#define __CHAR_BIT__ 8
#define __BIGGEST_ALIGNMENT__ 16
#define __ORDER_LITTLE_ENDIAN__ 1234
#define __ORDER_BIG_ENDIAN__ 4321
#define __ORDER_PDP_ENDIAN__ 3412
#define __BYTE_ORDER__ __ORDER_LITTLE_ENDIAN__
#define __FLOAT_WORD_ORDER__ __ORDER_LITTLE_ENDIAN__
#define __SIZEOF_POINTER__ 8
#define __GNUC_EXECUTION_CHARSET_NAME "UTF-8"
#define __GNUC_WIDE_EXECUTION_CHARSET_NAME "UTF-32LE"
#define __GNUG__ 11
#define __SIZE_TYPE__ long unsigned int
#define __PTRDIFF_TYPE__ long int
#define __WCHAR_TYPE__ int
#define __WINT_TYPE__ unsigned int
#define __INTMAX_TYPE__ long int
#define __UINTMAX_TYPE__ long unsigned int
#define __CHAR16_TYPE__ short unsigned int
#define __CHAR32_TYPE__ unsigned int
#define __SIG_ATOMIC_TYPE__ int
#define __INT8_TYPE__ signed char
#define __INT16_TYPE__ short int
#define __INT32_TYPE__ int
#define __INT64_TYPE__ long int
#define __UINT8_TYPE__ unsigned char
#define __UINT16_TYPE__ short unsigned int
#define __UINT32_TYPE__ unsigned int
#define __UINT64_TYPE__ long unsigned int
#define __INT_LEAST8_TYPE__ signed char
#define __INT_LEAST16_TYPE__ short int
#define __INT_LEAST32_TYPE__ int
#define __INT_LEAST64_TYPE__ long int
#define __UINT_LEAST8_TYPE__ unsigned char
#define __UINT_LEAST16_TYPE__ short unsigned int
#define __UINT_LEAST32_TYPE__ unsigned int
#define __UINT_LEAST64_TYPE__ long unsigned int
#define __INT_FAST8_TYPE__ signed char
#define __INT_FAST16_TYPE__ long int
#define __INT_FAST32_TYPE__ long int
#define __INT_FAST64_TYPE__ long int
#define __UINT_FAST8_TYPE__ unsigned char
#define __UINT_FAST16_TYPE__ long unsigned int
#define __UINT_FAST32_TYPE__ long unsigned int
#define __UINT_FAST64_TYPE__ long unsigned int
#define __INTPTR_TYPE__ long int
#define __UINTPTR_TYPE__ long unsigned int
#define __GXX_WEAK__ 1
#define __DEPRECATED 1
#define __GXX_RTTI 1
#define __cpp_rtti 199711L
#define __GXX_EXPERIMENTAL_CXX0X__ 1
#define __cpp_binary_literals 201304L
#define __cpp_hex_float 201603L
#define __cpp_runtime_arrays 198712L
#define __cpp_raw_strings 200710L
#define __cpp_unicode_literals 200710L
#define __cpp_user_defined_literals 200809L
#define __cpp_lambdas 200907L
#define __cpp_decltype 200707L
#define __cpp_attributes 200809L
#define __cpp_rvalue_reference 200610L
#define __cpp_rvalue_references 200610L
#define __cpp_variadic_templates 200704L
#define __cpp_initializer_lists 200806L
#define __cpp_delegating_constructors 200604L
#define __cpp_nsdmi 200809L
#define __cpp_inheriting_constructors 201511L
#define __cpp_ref_qualifiers 200710L
#define __cpp_alias_templates 200704L
#define __cpp_return_type_deduction 201304L
#define __cpp_init_captures 201304L
#define __cpp_generic_lambdas 201304L
#define __cpp_decltype_auto 201304L
#define __cpp_aggregate_nsdmi 201304L
#define __cpp_variable_templates 201304L
#define __cpp_digit_separators 201309L
#define __cpp_unicode_characters 201411L
#define __cpp_static_assert 201411L
#define __cpp_namespace_attributes 201411L
#define __cpp_enumerator_attributes 201411L
#define __cpp_nested_namespace_definitions 201411L
#define __cpp_fold_expressions 201603L
#define __cpp_nontype_template_args 201411L
#define __cpp_range_based_for 201603L
#define __cpp_constexpr 201603L
#define __cpp_if_constexpr 201606L
#define __cpp_capture_star_this 201603L
#define __cpp_inline_variables 201606L
#define __cpp_aggregate_bases 201603L
#define __cpp_deduction_guides 201703L
#define __cpp_noexcept_function_type 201510L
#define __cpp_template_auto 201606L
#define __cpp_structured_bindings 201606L
#define __cpp_variadic_using 201611L
#define __cpp_guaranteed_copy_elision 201606L
#define __cpp_nontype_template_parameter_auto 201606L
#define __cpp_modules 201810L
#define __cpp_sized_deallocation 201309L
#define __cpp_aligned_new 201606L
#define __STDCPP_DEFAULT_NEW_ALIGNMENT__ 16
#define __cpp_template_template_args 201611L
#define __cpp_threadsafe_static_init 200806L
#define __STDCPP_THREADS__ 1
#define __EXCEPTIONS 1
#define __cpp_exceptions 199711L
#define __GXX_ABI_VERSION 1015
#define __SCHAR_MAX__ 0x7f
#define __SHRT_MAX__ 0x7fff
#define __INT_MAX__ 0x7fffffff
#define __LONG_MAX__ 0x7fffffffffffffffL
#define __LONG_LONG_MAX__ 0x7fffffffffffffffLL
#define __WCHAR_MAX__ 0x7fffffff
#define __WCHAR_MIN__ (-__WCHAR_MAX__ - 1)
#define __WINT_MAX__ 0xffffffffU
#define __WINT_MIN__ 0U
#define __PTRDIFF_MAX__ 0x7fffffffffffffffL
#define __SIZE_MAX__ 0xffffffffffffffffUL
#define __SCHAR_WIDTH__ 8
#define __SHRT_WIDTH__ 16
#define __INT_WIDTH__ 32
#define __LONG_WIDTH__ 64
#define __LONG_LONG_WIDTH__ 64
#define __WCHAR_WIDTH__ 32
#define __WINT_WIDTH__ 32
#define __PTRDIFF_WIDTH__ 64
#define __SIZE_WIDTH__ 64
#define __INTMAX_MAX__ 0x7fffffffffffffffL
#define __INTMAX_C(c) c ## L
#define __UINTMAX_MAX__ 0xffffffffffffffffUL
#define __UINTMAX_C(c) c ## UL
#define __INTMAX_WIDTH__ 64
#define __SIG_ATOMIC_MAX__ 0x7fffffff
#define __SIG_ATOMIC_MIN__ (-__SIG_ATOMIC_MAX__ - 1)
#define __SIG_ATOMIC_WIDTH__ 32
#define __INT8_MAX__ 0x7f
#define __INT16_MAX__ 0x7fff
#define __INT32_MAX__ 0x7fffffff
#define __INT64_MAX__ 0x7fffffffffffffffL
#define __UINT8_MAX__ 0xff
#define __UINT16_MAX__ 0xffff
#define __UINT32_MAX__ 0xffffffffU
#define __UINT64_MAX__ 0xffffffffffffffffUL
#define __INT_LEAST8_MAX__ 0x7f
#define __INT8_C(c) c
#define __INT_LEAST8_WIDTH__ 8
#define __INT_LEAST16_MAX__ 0x7fff
#define __INT16_C(c) c
#define __INT_LEAST16_WIDTH__ 16
#define __INT_LEAST32_MAX__ 0x7fffffff
#define __INT32_C(c) c
#define __INT_LEAST32_WIDTH__ 32
#define __INT_LEAST64_MAX__ 0x7fffffffffffffffL
#define __INT64_C(c) c ## L
#define __INT_LEAST64_WIDTH__ 64
#define __UINT_LEAST8_MAX__ 0xff
#define __UINT8_C(c) c
#define __UINT_LEAST16_MAX__ 0xffff
#define __UINT16_C(c) c
#define __UINT_LEAST32_MAX__ 0xffffffffU
#define __UINT32_C(c) c ## U
#define __UINT_LEAST64_MAX__ 0xffffffffffffffffUL
#define __UINT64_C(c) c ## UL
#define __INT_FAST8_MAX__ 0x7f
#define __INT_FAST8_WIDTH__ 8
#define __INT_FAST16_MAX__ 0x7fffffffffffffffL
#define __INT_FAST16_WIDTH__ 64
#define __INT_FAST32_MAX__ 0x7fffffffffffffffL
#define __INT_FAST32_WIDTH__ 64
#define __INT_FAST64_MAX__ 0x7fffffffffffffffL
#define __INT_FAST64_WIDTH__ 64
#define __UINT_FAST8_MAX__ 0xff
#define __UINT_FAST16_MAX__ 0xffffffffffffffffUL
#define __UINT_FAST32_MAX__ 0xffffffffffffffffUL
#define __UINT_FAST64_MAX__ 0xffffffffffffffffUL
#define __INTPTR_MAX__ 0x7fffffffffffffffL
#define __INTPTR_WIDTH__ 64
#define __UINTPTR_MAX__ 0xffffffffffffffffUL
#define __GCC_IEC_559 2
#define __GCC_IEC_559_COMPLEX 2
#define __FLT_EVAL_METHOD__ 0
#define __FLT_EVAL_METHOD_TS_18661_3__ 0
#define __DEC_EVAL_METHOD__ 2
#define __FLT_RADIX__ 2
#define __FLT_MANT_DIG__ 24
#define __FLT_DIG__ 6
#define __FLT_MIN_EXP__ (-125)
#define __FLT_MIN_10_EXP__ (-37)
#define __FLT_MAX_EXP__ 128
#define __FLT_MAX_10_EXP__ 38
#define __FLT_DECIMAL_DIG__ 9
#define __FLT_MAX__ 3.40282346638528859811704183484516925e+38F
#define __FLT_NORM_MAX__ 3.40282346638528859811704183484516925e+38F
#define __FLT_MIN__ 1.17549435082228750796873653722224568e-38F
#define __FLT_EPSILON__ 1.19209289550781250000000000000000000e-7F
#define __FLT_DENORM_MIN__ 1.40129846432481707092372958328991613e-45F
#define __FLT_HAS_DENORM__ 1
#define __FLT_HAS_INFINITY__ 1
#define __FLT_HAS_QUIET_NAN__ 1
#define __FLT_IS_IEC_60559__ 2
#define __DBL_MANT_DIG__ 53
#define __DBL_DIG__ 15
#define __DBL_MIN_EXP__ (-1021)
#define __DBL_MIN_10_EXP__ (-307)
#define __DBL_MAX_EXP__ 1024
#define __DBL_MAX_10_EXP__ 308
#define __DBL_DECIMAL_DIG__ 17
#define __DBL_MAX__ double(1.79769313486231570814527423731704357e+308L)
#define __DBL_NORM_MAX__ double(1.79769313486231570814527423731704357e+308L)
#define __DBL_MIN__ double(2.22507385850720138309023271733240406e-308L)
#define __DBL_EPSILON__ double(2.22044604925031308084726333618164062e-16L)
#define __DBL_DENORM_MIN__ double(4.94065645841246544176568792868221372e-324L)
#define __DBL_HAS_DENORM__ 1
#define __DBL_HAS_INFINITY__ 1
#define __DBL_HAS_QUIET_NAN__ 1
#define __DBL_IS_IEC_60559__ 2
#define __LDBL_MANT_DIG__ 64
#define __LDBL_DIG__ 18
#define __LDBL_MIN_EXP__ (-16381)
#define __LDBL_MIN_10_EXP__ (-4931)
#define __LDBL_MAX_EXP__ 16384
#define __LDBL_MAX_10_EXP__ 4932
#define __DECIMAL_DIG__ 21
#define __LDBL_DECIMAL_DIG__ 21
#define __LDBL_MAX__ 1.18973149535723176502126385303097021e+4932L
#define __LDBL_NORM_MAX__ 1.18973149535723176502126385303097021e+4932L
#define __LDBL_MIN__ 3.36210314311209350626267781732175260e-4932L
#define __LDBL_EPSILON__ 1.08420217248550443400745280086994171e-19L
#define __LDBL_DENORM_MIN__ 3.64519953188247460252840593361941982e-4951L
#define __LDBL_HAS_DENORM__ 1
#define __LDBL_HAS_INFINITY__ 1
#define __LDBL_HAS_QUIET_NAN__ 1
#define __LDBL_IS_IEC_60559__ 2
#define __FLT32_MANT_DIG__ 24
#define __FLT32_DIG__ 6
#define __FLT32_MIN_EXP__ (-125)
#define __FLT32_MIN_10_EXP__ (-37)
#define __FLT32_MAX_EXP__ 128
#define __FLT32_MAX_10_EXP__ 38
#define __FLT32_DECIMAL_DIG__ 9
#define __FLT32_MAX__ 3.40282346638528859811704183484516925e+38F32
#define __FLT32_NORM_MAX__ 3.40282346638528859811704183484516925e+38F32
#define __FLT32_MIN__ 1.17549435082228750796873653722224568e-38F32
#define __FLT32_EPSILON__ 1.19209289550781250000000000000000000e-7F32
#define __FLT32_DENORM_MIN__ 1.40129846432481707092372958328991613e-45F32
#define __FLT32_HAS_DENORM__ 1
#define __FLT32_HAS_INFINITY__ 1
#define __FLT32_HAS_QUIET_NAN__ 1
#define __FLT32_IS_IEC_60559__ 2
#define __FLT64_MANT_DIG__ 53
#define __FLT64_DIG__ 15
#define __FLT64_MIN_EXP__ (-1021)
#define __FLT64_MIN_10_EXP__ (-307)
#define __FLT64_MAX_EXP__ 1024
#define __FLT64_MAX_10_EXP__ 308
#define __FLT64_DECIMAL_DIG__ 17
#define __FLT64_MAX__ 1.79769313486231570814527423731704357e+308F64
#define __FLT64_NORM_MAX__ 1.79769313486231570814527423731704357e+308F64
#define __FLT64_MIN__ 2.22507385850720138309023271733240406e-308F64
#define __FLT64_EPSILON__ 2.22044604925031308084726333618164062e-16F64
#define __FLT64_DENORM_MIN__ 4.94065645841246544176568792868221372e-324F64
#define __FLT64_HAS_DENORM__ 1
#define __FLT64_HAS_INFINITY__ 1
#define __FLT64_HAS_QUIET_NAN__ 1
#define __FLT64_IS_IEC_60559__ 2
#define __FLT128_MANT_DIG__ 113
#define __FLT128_DIG__ 33
#define __FLT128_MIN_EXP__ (-16381)
#define __FLT128_MIN_10_EXP__ (-4931)
#define __FLT128_MAX_EXP__ 16384
#define __FLT128_MAX_10_EXP__ 4932
#define __FLT128_DECIMAL_DIG__ 36
#define __FLT128_MAX__ 1.18973149535723176508575932662800702e+4932F128
#define __FLT128_NORM_MAX__ 1.18973149535723176508575932662800702e+4932F128
#define __FLT128_MIN__ 3.36210314311209350626267781732175260e-4932F128
#define __FLT128_EPSILON__ 1.92592994438723585305597794258492732e-34F128
#define __FLT128_DENORM_MIN__ 6.47517511943802511092443895822764655e-4966F128
#define __FLT128_HAS_DENORM__ 1
#define __FLT128_HAS_INFINITY__ 1
#define __FLT128_HAS_QUIET_NAN__ 1
#define __FLT128_IS_IEC_60559__ 2
#define __FLT32X_MANT_DIG__ 53
#define __FLT32X_DIG__ 15
#define __FLT32X_MIN_EXP__ (-1021)
#define __FLT32X_MIN_10_EXP__ (-307)
#define __FLT32X_MAX_EXP__ 1024
#define __FLT32X_MAX_10_EXP__ 308
#define __FLT32X_DECIMAL_DIG__ 17
#define __FLT32X_MAX__ 1.79769313486231570814527423731704357e+308F32x
#define __FLT32X_NORM_MAX__ 1.79769313486231570814527423731704357e+308F32x
#define __FLT32X_MIN__ 2.22507385850720138309023271733240406e-308F32x
#define __FLT32X_EPSILON__ 2.22044604925031308084726333618164062e-16F32x
#define __FLT32X_DENORM_MIN__ 4.94065645841246544176568792868221372e-324F32x
#define __FLT32X_HAS_DENORM__ 1
#define __FLT32X_HAS_INFINITY__ 1
#define __FLT32X_HAS_QUIET_NAN__ 1
#define __FLT32X_IS_IEC_60559__ 2
#define __FLT64X_MANT_DIG__ 64
#define __FLT64X_DIG__ 18
#define __FLT64X_MIN_EXP__ (-16381)
#define __FLT64X_MIN_10_EXP__ (-4931)
#define __FLT64X_MAX_EXP__ 16384
#define __FLT64X_MAX_10_EXP__ 4932
#define __FLT64X_DECIMAL_DIG__ 21
#define __FLT64X_MAX__ 1.18973149535723176502126385303097021e+4932F64x
#define __FLT64X_NORM_MAX__ 1.18973149535723176502126385303097021e+4932F64x
#define __FLT64X_MIN__ 3.36210314311209350626267781732175260e-4932F64x
#define __FLT64X_EPSILON__ 1.08420217248550443400745280086994171e-19F64x
#define __FLT64X_DENORM_MIN__ 3.64519953188247460252840593361941982e-4951F64x
#define __FLT64X_HAS_DENORM__ 1
#define __FLT64X_HAS_INFINITY__ 1
#define __FLT64X_HAS_QUIET_NAN__ 1
#define __FLT64X_IS_IEC_60559__ 2
#define __DEC32_MANT_DIG__ 7
#define __DEC32_MIN_EXP__ (-94)
#define __DEC32_MAX_EXP__ 97
#define __DEC32_MIN__ 1E-95DF
#define __DEC32_MAX__ 9.999999E96DF
#define __DEC32_EPSILON__ 1E-6DF
#define __DEC32_SUBNORMAL_MIN__ 0.000001E-95DF
#define __DEC64_MANT_DIG__ 16
#define __DEC64_MIN_EXP__ (-382)
#define __DEC64_MAX_EXP__ 385
#define __DEC64_MIN__ 1E-383DD
#define __DEC64_MAX__ 9.999999999999999E384DD
#define __DEC64_EPSILON__ 1E-15DD
#define __DEC64_SUBNORMAL_MIN__ 0.000000000000001E-383DD
#define __DEC128_MANT_DIG__ 34
#define __DEC128_MIN_EXP__ (-6142)
#define __DEC128_MAX_EXP__ 6145
#define __DEC128_MIN__ 1E-6143DL
#define __DEC128_MAX__ 9.999999999999999999999999999999999E6144DL
#define __DEC128_EPSILON__ 1E-33DL
#define __DEC128_SUBNORMAL_MIN__ 0.000000000000000000000000000000001E-6143DL
#define __REGISTER_PREFIX__ 
#define __USER_LABEL_PREFIX__ 
#define __GNUC_STDC_INLINE__ 1
#define __NO_INLINE__ 1
#define __STRICT_ANSI__ 1
#define __GCC_HAVE_SYNC_COMPARE_AND_SWAP_1 1
#define __GCC_HAVE_SYNC_COMPARE_AND_SWAP_2 1
#define __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4 1
#define __GCC_HAVE_SYNC_COMPARE_AND_SWAP_8 1
#define __GCC_ATOMIC_BOOL_LOCK_FREE 2
#define __GCC_ATOMIC_CHAR_LOCK_FREE 2
#define __GCC_ATOMIC_CHAR16_T_LOCK_FREE 2
#define __GCC_ATOMIC_CHAR32_T_LOCK_FREE 2
#define __GCC_ATOMIC_WCHAR_T_LOCK_FREE 2
#define __GCC_ATOMIC_SHORT_LOCK_FREE 2
#define __GCC_ATOMIC_INT_LOCK_FREE 2
#define __GCC_ATOMIC_LONG_LOCK_FREE 2
#define __GCC_ATOMIC_LLONG_LOCK_FREE 2
#define __GCC_ATOMIC_TEST_AND_SET_TRUEVAL 1
#define __GCC_ATOMIC_POINTER_LOCK_FREE 2
#define __HAVE_SPECULATION_SAFE_VALUE 1
#define __GCC_HAVE_DWARF2_CFI_ASM 1
#define __PRAGMA_REDEFINE_EXTNAME 1
#define __SIZEOF_INT128__ 16
#define __SIZEOF_WCHAR_T__ 4
#define __SIZEOF_WINT_T__ 4
#define __SIZEOF_PTRDIFF_T__ 8
#define __amd64 1
#define __amd64__ 1
#define __x86_64 1
#define __x86_64__ 1
#define __SIZEOF_FLOAT80__ 16
#define __SIZEOF_FLOAT128__ 16
#define __ATOMIC_HLE_ACQUIRE 65536
#define __ATOMIC_HLE_RELEASE 131072
#define __GCC_ASM_FLAG_OUTPUTS__ 1
#define __k8 1
#define __k8__ 1
#define __code_model_small__ 1
#define __MMX__ 1
#define __SSE__ 1
#define __SSE2__ 1
#define __FXSR__ 1
#define __SSE_MATH__ 1
#define __SSE2_MATH__ 1
#define __MMX_WITH_SSE__ 1
#define __SEG_FS 1
#define __SEG_GS 1
#define __gnu_linux__ 1
#define __linux 1
#define __linux__ 1
#define __unix 1
#define __unix__ 1
#define __ELF__ 1
#define __DECIMAL_BID_FORMAT__ 1
# 0 "<command-line>"
#define _GNU_SOURCE 1
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _STDC_PREDEF_H 1

/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */

/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */



#define __STDC_IEC_559__ 1







#define __STDC_IEC_559_COMPLEX__ 1





/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
#define __STDC_ISO_10646__ 201706L

# 0 "<command-line>" 2
# 482 "iostream"
// Standard iostream objects -*- C++ -*-

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/iostream
 *  This is a Standard C++ Library header.
 */

//
// ISO C++ 14882: 27.3  Standard iostream objects
//


#define _GLIBCXX_IOSTREAM 1

       

// Predefined symbols and macros -*- C++ -*-

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/c++config.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{version}
 */


#define _GLIBCXX_CXX_CONFIG_H 1

// The major release number for the GCC release the C++ library belongs to.
#define _GLIBCXX_RELEASE 11

// The datestamp of the C++ library in compressed ISO date format.
#define __GLIBCXX__ 20210217

// Macros for various attributes.
//   _GLIBCXX_PURE
//   _GLIBCXX_CONST
//   _GLIBCXX_NORETURN
//   _GLIBCXX_NOTHROW
//   _GLIBCXX_VISIBILITY

#define _GLIBCXX_PURE __attribute__ ((__pure__))



#define _GLIBCXX_CONST __attribute__ ((__const__))



#define _GLIBCXX_NORETURN __attribute__ ((__noreturn__))


// See below for C++






// Macros for visibility attributes.
//   _GLIBCXX_HAVE_ATTRIBUTE_VISIBILITY
//   _GLIBCXX_VISIBILITY
#define _GLIBCXX_HAVE_ATTRIBUTE_VISIBILITY 1


#define _GLIBCXX_VISIBILITY(V) __attribute__ ((__visibility__ (#V)))
#define _GLIBCXX_VISIBILITY2(V) __attribute__ ((__visibility__ (#V)))






// Macros for deprecated attributes.
//   _GLIBCXX_USE_DEPRECATED
//   _GLIBCXX_DEPRECATED
//   _GLIBCXX_DEPRECATED_SUGGEST( string-literal )
//   _GLIBCXX11_DEPRECATED
//   _GLIBCXX11_DEPRECATED_SUGGEST( string-literal )
//   _GLIBCXX17_DEPRECATED
//   _GLIBCXX17_DEPRECATED_SUGGEST( string-literal )
//   _GLIBCXX20_DEPRECATED( string-literal )
//   _GLIBCXX20_DEPRECATED_SUGGEST( string-literal )

#define _GLIBCXX_USE_DEPRECATED 1



#define _GLIBCXX_DEPRECATED __attribute__ ((__deprecated__))
#define _GLIBCXX_DEPRECATED_SUGGEST(ALT) __attribute__ ((__deprecated__ ("use '" ALT "' instead")))







#define _GLIBCXX11_DEPRECATED _GLIBCXX_DEPRECATED
#define _GLIBCXX11_DEPRECATED_SUGGEST(ALT) _GLIBCXX_DEPRECATED_SUGGEST(ALT)






#define _GLIBCXX17_DEPRECATED [[__deprecated__]]
#define _GLIBCXX17_DEPRECATED_SUGGEST(ALT) _GLIBCXX_DEPRECATED_SUGGEST(ALT)









#define _GLIBCXX20_DEPRECATED(MSG) 
#define _GLIBCXX20_DEPRECATED_SUGGEST(ALT) 


// Macros for ABI tag attributes.

#define _GLIBCXX_ABI_TAG_CXX11 __attribute ((__abi_tag__ ("cxx11")))


// Macro to warn about unused results.

#define _GLIBCXX_NODISCARD [[__nodiscard__]]








// Macro for constexpr, to support in mixed 03/0x mode.


#define _GLIBCXX_CONSTEXPR constexpr
#define _GLIBCXX_USE_CONSTEXPR constexpr








#define _GLIBCXX14_CONSTEXPR constexpr







#define _GLIBCXX17_CONSTEXPR constexpr









#define _GLIBCXX20_CONSTEXPR 





#define _GLIBCXX17_INLINE inline





// Macro for noexcept, to support in mixed 03/0x mode.


#define _GLIBCXX_NOEXCEPT noexcept
#define _GLIBCXX_NOEXCEPT_IF(...) noexcept(__VA_ARGS__)
#define _GLIBCXX_USE_NOEXCEPT noexcept
#define _GLIBCXX_THROW(_EXC) 









#define _GLIBCXX_NOTHROW _GLIBCXX_USE_NOEXCEPT




#define _GLIBCXX_THROW_OR_ABORT(_EXC) (throw (_EXC))






#define _GLIBCXX_NOEXCEPT_PARM , bool _NE
#define _GLIBCXX_NOEXCEPT_QUAL noexcept (_NE)





// Macro for extern template, ie controlling template linkage via use
// of extern keyword on template declaration. As documented in the g++
// manual, it inhibits all implicit instantiations and is used
// throughout the library to avoid multiple weak definitions for
// required types that are already explicitly instantiated in the
// library binary. This substantially reduces the binary size of
// resulting executables.
// Special case: _GLIBCXX_EXTERN_TEMPLATE == -1 disallows extern
// templates only in basic_string, thus activating its debug-mode
// checks even at -O0.
#define _GLIBCXX_EXTERN_TEMPLATE 1

/*
  Outline of libstdc++ namespaces.

  namespace std
  {
    namespace __debug { }
    namespace __parallel { }
    namespace __cxx1998 { }

    namespace __detail {
      namespace __variant { }				// C++17
    }

    namespace rel_ops { }

    namespace tr1
    {
      namespace placeholders { }
      namespace regex_constants { }
      namespace __detail { }
    }

    namespace tr2 { }
    
    namespace decimal { }

    namespace chrono { }				// C++11
    namespace placeholders { }				// C++11
    namespace regex_constants { }			// C++11
    namespace this_thread { }				// C++11
    inline namespace literals {				// C++14
      inline namespace chrono_literals { }		// C++14
      inline namespace complex_literals { }		// C++14
      inline namespace string_literals { }		// C++14
      inline namespace string_view_literals { }		// C++17
    }
  }

  namespace abi { }

  namespace __gnu_cxx
  {
    namespace __detail { }
  }

  For full details see:
  http://gcc.gnu.org/onlinedocs/libstdc++/latest-doxygen/namespaces.html
*/
namespace std
{
  typedef __SIZE_TYPE__ 	size_t;
  typedef __PTRDIFF_TYPE__	ptrdiff_t;


  typedef decltype(nullptr)	nullptr_t;

}

#define _GLIBCXX_USE_DUAL_ABI 1







#define _GLIBCXX_USE_CXX11_ABI 1



namespace std
{
  inline namespace __cxx11 __attribute__((__abi_tag__ ("cxx11"))) { }
}
namespace __gnu_cxx
{
  inline namespace __cxx11 __attribute__((__abi_tag__ ("cxx11"))) { }
}
#define _GLIBCXX_NAMESPACE_CXX11 __cxx11::
#define _GLIBCXX_BEGIN_NAMESPACE_CXX11 namespace __cxx11 {
#define _GLIBCXX_END_NAMESPACE_CXX11 }
#define _GLIBCXX_DEFAULT_ABI_TAG _GLIBCXX_ABI_TAG_CXX11







// Defined if inline namespaces are used for versioning.
#define _GLIBCXX_INLINE_VERSION 0

// Inline namespace for symbol versioning.
#define _GLIBCXX_BEGIN_NAMESPACE_VERSION 
#define _GLIBCXX_END_NAMESPACE_VERSION 


// Inline namespaces for special modes: debug, parallel.

// Macros for namespace scope. Either namespace std:: or the name
// of some nested namespace within it corresponding to the active mode.
// _GLIBCXX_STD_A
// _GLIBCXX_STD_C
//
// Macros for opening/closing conditional namespaces.
// _GLIBCXX_BEGIN_NAMESPACE_ALGO
// _GLIBCXX_END_NAMESPACE_ALGO
// _GLIBCXX_BEGIN_NAMESPACE_CONTAINER
// _GLIBCXX_END_NAMESPACE_CONTAINER






#define _GLIBCXX_STD_C std
#define _GLIBCXX_BEGIN_NAMESPACE_CONTAINER 
#define _GLIBCXX_END_NAMESPACE_CONTAINER 








#define _GLIBCXX_STD_A std
#define _GLIBCXX_BEGIN_NAMESPACE_ALGO 
#define _GLIBCXX_END_NAMESPACE_ALGO 


// GLIBCXX_ABI Deprecated
// Define if compatibility should be provided for -mlong-double-64.
#undef _GLIBCXX_LONG_DOUBLE_COMPAT
// Define if compatibility should be provided for alternative 128-bit long
// double formats.
#undef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT

// Inline namespaces for long double 128 modes.

#define _GLIBCXX_NAMESPACE_LDBL 
#define _GLIBCXX_BEGIN_NAMESPACE_LDBL 
#define _GLIBCXX_END_NAMESPACE_LDBL 



#define _GLIBCXX_NAMESPACE_LDBL_OR_CXX11 _GLIBCXX_NAMESPACE_CXX11
#define _GLIBCXX_BEGIN_NAMESPACE_LDBL_OR_CXX11 _GLIBCXX_BEGIN_NAMESPACE_CXX11
#define _GLIBCXX_END_NAMESPACE_LDBL_OR_CXX11 _GLIBCXX_END_NAMESPACE_CXX11








// Debug Mode implies checking assertions.




// Disable std::string explicit instantiation declarations in order to assert.





// Assert.




#define __glibcxx_assert_2(_Condition) 


// Macros for race detectors.
// _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(A) and
// _GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(A) should be used to explain
// atomic (lock-free) synchronization to race detectors:
// the race detector will infer a happens-before arc from the former to the
// latter when they share the same argument pointer.
//
// The most frequent use case for these macros (and the only case in the
// current implementation of the library) is atomic reference counting:
//   void _M_remove_reference()
//   {
//     _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(&this->_M_refcount);
//     if (__gnu_cxx::__exchange_and_add_dispatch(&this->_M_refcount, -1) <= 0)
//       {
//         _GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(&this->_M_refcount);
//         _M_destroy(__a);
//       }
//   }
// The annotations in this example tell the race detector that all memory
// accesses occurred when the refcount was positive do not race with
// memory accesses which occurred after the refcount became zero.

#define _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(A) 


#define _GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(A) 


// Macros for C linkage: define extern "C" linkage only when using C++.
#define _GLIBCXX_BEGIN_EXTERN_C extern "C" {
#define _GLIBCXX_END_EXTERN_C }

#define _GLIBCXX_USE_ALLOCATOR_NEW 1
















// First includes.

// Pick up any OS-specific definitions.
// Specific definitions for GNU/Linux  -*- C++ -*-

// Copyright (C) 2000-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/os_defines.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iosfwd}
 */


#define _GLIBCXX_OS_DEFINES 1

// System-specific #define, typedefs, corrections, etc, go here.  This
// file will come before all others.

// This keeps isanum, et al from being propagated as macros.
#define __NO_CTYPE 1

/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _FEATURES_H 1

/* These are defined by the user (or the compiler)
   to specify the desired environment:

   __STRICT_ANSI__	ISO Standard C.
   _ISOC99_SOURCE	Extensions to ISO C89 from ISO C99.
   _ISOC11_SOURCE	Extensions to ISO C99 from ISO C11.
   __STDC_WANT_LIB_EXT2__
			Extensions to ISO C99 from TR 27431-2:2010.
   __STDC_WANT_IEC_60559_BFP_EXT__
			Extensions to ISO C11 from TS 18661-1:2014.
   __STDC_WANT_IEC_60559_FUNCS_EXT__
			Extensions to ISO C11 from TS 18661-4:2015.
   __STDC_WANT_IEC_60559_TYPES_EXT__
			Extensions to ISO C11 from TS 18661-3:2015.

   _POSIX_SOURCE	IEEE Std 1003.1.
   _POSIX_C_SOURCE	If ==1, like _POSIX_SOURCE; if >=2 add IEEE Std 1003.2;
			if >=199309L, add IEEE Std 1003.1b-1993;
			if >=199506L, add IEEE Std 1003.1c-1995;
			if >=200112L, all of IEEE 1003.1-2004
			if >=200809L, all of IEEE 1003.1-2008
   _XOPEN_SOURCE	Includes POSIX and XPG things.  Set to 500 if
			Single Unix conformance is wanted, to 600 for the
			sixth revision, to 700 for the seventh revision.
   _XOPEN_SOURCE_EXTENDED XPG things and X/Open Unix extensions.
   _LARGEFILE_SOURCE	Some more functions for correct standard I/O.
   _LARGEFILE64_SOURCE	Additional functionality from LFS for large files.
   _FILE_OFFSET_BITS=N	Select default filesystem interface.
   _ATFILE_SOURCE	Additional *at interfaces.
   _GNU_SOURCE		All of the above, plus GNU extensions.
   _DEFAULT_SOURCE	The default set of features (taking precedence over
			__STRICT_ANSI__).

   _FORTIFY_SOURCE	Add security hardening to many library functions.
			Set to 1 or 2; 2 performs stricter checks than 1.

   _REENTRANT, _THREAD_SAFE
			Obsolete; equivalent to _POSIX_C_SOURCE=199506L.

   The `-ansi' switch to the GNU C compiler, and standards conformance
   options such as `-std=c99', define __STRICT_ANSI__.  If none of
   these are defined, or if _DEFAULT_SOURCE is defined, the default is
   to have _POSIX_SOURCE set to one and _POSIX_C_SOURCE set to
   200809L, as well as enabling miscellaneous functions from BSD and
   SVID.  If more than one of these are defined, they accumulate.  For
   example __STRICT_ANSI__, _POSIX_SOURCE and _POSIX_C_SOURCE together
   give you ISO C, 1003.1, and 1003.2, but nothing else.

   These are defined by this file and are used by the
   header files to decide what to declare or define:

   __GLIBC_USE (F)	Define things from feature set F.  This is defined
			to 1 or 0; the subsequent macros are either defined
			or undefined, and those tests should be moved to
			__GLIBC_USE.
   __USE_ISOC11		Define ISO C11 things.
   __USE_ISOC99		Define ISO C99 things.
   __USE_ISOC95		Define ISO C90 AMD1 (C95) things.
   __USE_ISOCXX11	Define ISO C++11 things.
   __USE_POSIX		Define IEEE Std 1003.1 things.
   __USE_POSIX2		Define IEEE Std 1003.2 things.
   __USE_POSIX199309	Define IEEE Std 1003.1, and .1b things.
   __USE_POSIX199506	Define IEEE Std 1003.1, .1b, .1c and .1i things.
   __USE_XOPEN		Define XPG things.
   __USE_XOPEN_EXTENDED	Define X/Open Unix things.
   __USE_UNIX98		Define Single Unix V2 things.
   __USE_XOPEN2K        Define XPG6 things.
   __USE_XOPEN2KXSI     Define XPG6 XSI things.
   __USE_XOPEN2K8       Define XPG7 things.
   __USE_XOPEN2K8XSI    Define XPG7 XSI things.
   __USE_LARGEFILE	Define correct standard I/O things.
   __USE_LARGEFILE64	Define LFS things with separate names.
   __USE_FILE_OFFSET64	Define 64bit interface as default.
   __USE_MISC		Define things from 4.3BSD or System V Unix.
   __USE_ATFILE		Define *at interfaces and AT_* constants for them.
   __USE_GNU		Define GNU extensions.
   __USE_FORTIFY_LEVEL	Additional security measures used, according to level.

   The macros `__GNU_LIBRARY__', `__GLIBC__', and `__GLIBC_MINOR__' are
   defined by this file unconditionally.  `__GNU_LIBRARY__' is provided
   only for compatibility.  All new code should use the other symbols
   to test for features.

   All macros listed above as possibly being defined by this file are
   explicitly undefined if they are not explicitly defined.
   Feature-test macros that are not defined by the user or compiler
   but are implied by the other feature-test macros defined (or by the
   lack of any definitions) are defined by the file.

   ISO C feature test macros depend on the definition of the macro
   when an affected header is included, not when the first system
   header is included, and so they are handled in
   <bits/libc-header-start.h>, which does not have a multiple include
   guard.  Feature test macros that can be handled from the first
   system header included are handled here.  */


/* Undefine everything, so we get a clean slate.  */
#undef __USE_ISOC11
#undef __USE_ISOC99
#undef __USE_ISOC95
#undef __USE_ISOCXX11
#undef __USE_POSIX
#undef __USE_POSIX2
#undef __USE_POSIX199309
#undef __USE_POSIX199506
#undef __USE_XOPEN
#undef __USE_XOPEN_EXTENDED
#undef __USE_UNIX98
#undef __USE_XOPEN2K
#undef __USE_XOPEN2KXSI
#undef __USE_XOPEN2K8
#undef __USE_XOPEN2K8XSI
#undef __USE_LARGEFILE
#undef __USE_LARGEFILE64
#undef __USE_FILE_OFFSET64
#undef __USE_MISC
#undef __USE_ATFILE
#undef __USE_GNU
#undef __USE_FORTIFY_LEVEL
#undef __KERNEL_STRICT_NAMES
#undef __GLIBC_USE_DEPRECATED_GETS

/* Suppress kernel-name space pollution unless user expressedly asks
   for it.  */

#define __KERNEL_STRICT_NAMES 


/* Convenience macro to test the version of gcc.
   Use like this:
   #if __GNUC_PREREQ (2,8)
   ... code requiring gcc 2.8 or later ...
   #endif
   Note: only works for GCC 2.0 and later, because __GNUC_MINOR__ was
   added in 2.0.  */

#define __GNUC_PREREQ(maj,min) ((__GNUC__ << 16) + __GNUC_MINOR__ >= ((maj) << 16) + (min))





/* Similarly for clang.  Features added to GCC after version 4.2 may
   or may not also be available in clang, and clang's definitions of
   __GNUC(_MINOR)__ are fixed at 4 and 2 respectively.  Not all such
   features can be queried via __has_extension/__has_feature.  */




#define __glibc_clang_prereq(maj,min) 0


/* Whether to use feature set F.  */
#define __GLIBC_USE(F) __GLIBC_USE_ ## F

/* _BSD_SOURCE and _SVID_SOURCE are deprecated aliases for
   _DEFAULT_SOURCE.  If _DEFAULT_SOURCE is present we do not
   issue a warning; the expectation is that the source is being
   transitioned to use the new macro.  */







/* If _GNU_SOURCE was defined by the user, turn on all the other features.  */

#undef _ISOC95_SOURCE
#define _ISOC95_SOURCE 1
#undef _ISOC99_SOURCE
#define _ISOC99_SOURCE 1
#undef _ISOC11_SOURCE
#define _ISOC11_SOURCE 1
#undef _POSIX_SOURCE
#define _POSIX_SOURCE 1
#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#undef _XOPEN_SOURCE
#define _XOPEN_SOURCE 700
#undef _XOPEN_SOURCE_EXTENDED
#define _XOPEN_SOURCE_EXTENDED 1
#undef _LARGEFILE64_SOURCE
#define _LARGEFILE64_SOURCE 1
#undef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1
#undef _ATFILE_SOURCE
#define _ATFILE_SOURCE 1


/* If nothing (other than _GNU_SOURCE and _DEFAULT_SOURCE) is defined,
   define _DEFAULT_SOURCE.  */





#undef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1


/* This is to enable the ISO C11 extension.  */


#define __USE_ISOC11 1


/* This is to enable the ISO C99 extension.  */


#define __USE_ISOC99 1


/* This is to enable the ISO C90 Amendment 1:1995 extension.  */


#define __USE_ISOC95 1



/* This is to enable compatibility for ISO C++17.  */

#define __USE_ISOC11 1

/* This is to enable compatibility for ISO C++11.
   Check the temporary macro for now, too.  */

#define __USE_ISOCXX11 1
#define __USE_ISOC99 1



/* If none of the ANSI/POSIX macros are defined, or if _DEFAULT_SOURCE
   is defined, use POSIX.1-2008 (or another version depending on
   _XOPEN_SOURCE).  */




#undef _POSIX_SOURCE
#define _POSIX_SOURCE 1
#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L



/* Some C libraries once required _REENTRANT and/or _THREAD_SAFE to be
   defined in all multithreaded code.  GNU libc has not required this
   for many years.  We now treat them as compatibility synonyms for
   _POSIX_C_SOURCE=199506L, which is the earliest level of POSIX with
   comprehensive support for multithreaded code.  Using them never
   lowers the selected level of POSIX conformance, only raises it.  */










#define __USE_POSIX 1



#define __USE_POSIX2 1



#define __USE_POSIX199309 1



#define __USE_POSIX199506 1



#define __USE_XOPEN2K 1
#undef __USE_ISOC95
#define __USE_ISOC95 1
#undef __USE_ISOC99
#define __USE_ISOC99 1



#define __USE_XOPEN2K8 1
#undef _ATFILE_SOURCE
#define _ATFILE_SOURCE 1



#define __USE_XOPEN 1

#define __USE_XOPEN_EXTENDED 1
#define __USE_UNIX98 1
#undef _LARGEFILE_SOURCE
#define _LARGEFILE_SOURCE 1


#define __USE_XOPEN2K8 1
#define __USE_XOPEN2K8XSI 1

#define __USE_XOPEN2K 1
#define __USE_XOPEN2KXSI 1
#undef __USE_ISOC95
#define __USE_ISOC95 1
#undef __USE_ISOC99
#define __USE_ISOC99 1









#define __USE_LARGEFILE 1



#define __USE_LARGEFILE64 1







#define __USE_MISC 1



#define __USE_ATFILE 1



#define __USE_GNU 1



#define __USE_FORTIFY_LEVEL 0


/* The function 'gets' existed in C89, but is impossible to use
   safely.  It has been removed from ISO C11 and ISO C++14.  Note: for
   compatibility with various implementations of <cstdio>, this test
   must consider only the value of __cplusplus when compiling C++.  */

#define __GLIBC_USE_DEPRECATED_GETS 0




/* Get definitions of __STDC_* predefined macros, if the compiler has
   not preincluded this header automatically.  */


/* This macro indicates that the installed library is the GNU C Library.
   For historic reasons the value now is 6 and this will stay from now
   on.  The use of this variable is deprecated.  Use __GLIBC__ and
   __GLIBC_MINOR__ now (see below) when you want to test for a specific
   GNU C library version and use the values in <gnu/lib-names.h> to get
   the sonames of the shared libraries.  */
#undef __GNU_LIBRARY__
#define __GNU_LIBRARY__ 6

/* Major and minor version number of the GNU C library package.  Use
   these macros to test for features in specific releases.  */
#define __GLIBC__ 2
#define __GLIBC_MINOR__ 28

#define __GLIBC_PREREQ(maj,min) ((__GLIBC__ << 16) + __GLIBC_MINOR__ >= ((maj) << 16) + (min))


/* This is here only because every header file already includes this one.  */


/* Copyright (C) 1992-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _SYS_CDEFS_H 1

/* We are almost always included from features.h. */




/* The GNU libc does not support any K&R compilers or the traditional mode
   of ISO C compilers anymore.  Check for some of the combinations not
   anymore supported.  */




/* Some user header file might have defined this before.  */
#undef __P
#undef __PMT



/* All functions, except those with callbacks or those that
   synchronize memory, are leaf functions.  */

#define __LEAF , __leaf__
#define __LEAF_ATTR __attribute__ ((__leaf__))





/* GCC can always grok prototypes.  For C++ programs we add throw()
   to help it optimize the function calls.  But this works only with
   gcc 2.8.x and egcs.  For gcc 3.2 and up we even mark C functions
   as non-throwing using a function attribute since programs can use
   the -fexceptions options for C code as well.  */







#define __THROW throw ()
#define __THROWNL throw ()
#define __NTH(fct) __LEAF_ATTR fct throw ()
#define __NTHNL(fct) fct throw ()









/* Compilers that are not clang may object to
       #if defined __clang__ && __has_extension(...)
   even though they do not need to evaluate the right-hand side of the &&.  */



#define __glibc_clang_has_extension(ext) 0


/* These two macros are not used in glibc anymore.  They are kept here
   only because some other projects expect the macros to be defined.  */
#define __P(args) args
#define __PMT(args) args

/* For these things, GCC behaves the ANSI way normally,
   and the non-ANSI way under -traditional.  */

#define __CONCAT(x,y) x ## y
#define __STRING(x) #x

/* This is not a typedef so `const __ptr_t' does the right thing.  */
#define __ptr_t void *


/* C++ needs to know that types and declarations are C, not C++.  */

#define __BEGIN_DECLS extern "C" {
#define __END_DECLS }






/* Fortify support.  */
#define __bos(ptr) __builtin_object_size (ptr, __USE_FORTIFY_LEVEL > 1)
#define __bos0(ptr) __builtin_object_size (ptr, 0)


#define __warndecl(name,msg) extern void name (void) __attribute__((__warning__ (msg)))

#define __warnattr(msg) __attribute__((__warning__ (msg)))
#define __errordecl(name,msg) extern void name (void) __attribute__((__error__ (msg)))







/* Support for flexible arrays.
   Headers that should use flexible arrays only if they're "real"
   (e.g. only if they won't affect sizeof()) should test
   #if __glibc_c99_flexarr_available.  */




/* GCC 2.97 supports C99 flexible array members as an extension,
   even when in C89 mode or compiling C++ (any version).  */
#define __flexarr []
#define __glibc_c99_flexarr_available 1


/* __asm__ ("xyz") is used throughout the headers to rename functions
   at the assembly language level.  This is wrapped by the __REDIRECT
   macro, in order to support compilers that can do this some other
   way.  When compilers don't support asm-names at all, we have to do
   preprocessor tricks instead (which don't have exactly the right
   semantics, but it's the best we can do).

   Example:
   int __REDIRECT(setpgrp, (__pid_t pid, __pid_t pgrp), setpgid); */



#define __REDIRECT(name,proto,alias) name proto __asm__ (__ASMNAME (#alias))

#define __REDIRECT_NTH(name,proto,alias) name proto __THROW __asm__ (__ASMNAME (#alias))

#define __REDIRECT_NTHNL(name,proto,alias) name proto __THROWNL __asm__ (__ASMNAME (#alias))







#define __ASMNAME(cname) __ASMNAME2 (__USER_LABEL_PREFIX__, cname)
#define __ASMNAME2(prefix,cname) __STRING (prefix) cname

/*
#elif __SOME_OTHER_COMPILER__

	_Pragma("let " #name " = " #alias)
*/


/* GCC has various useful declarations that can be made with the
   `__attribute__' syntax.  All of the ways we use this do fine if
   they are omitted for compilers that don't understand it. */




/* At some point during the gcc 2.96 development the `malloc' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.  */

#define __attribute_malloc__ __attribute__ ((__malloc__))




/* Tell the compiler which arguments to an allocation function
   indicate the size of the allocation.  */

#define __attribute_alloc_size__(params) __attribute__ ((__alloc_size__ params))





/* At some point during the gcc 2.96 development the `pure' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.  */

#define __attribute_pure__ __attribute__ ((__pure__))




/* This declaration tells the compiler that the value is constant.  */

#define __attribute_const__ __attribute__ ((__const__))




/* At some point during the gcc 3.1 development the `used' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.  */

#define __attribute_used__ __attribute__ ((__used__))
#define __attribute_noinline__ __attribute__ ((__noinline__))





/* Since version 3.2, gcc allows marking deprecated functions.  */

#define __attribute_deprecated__ __attribute__ ((__deprecated__))




/* Since version 4.5, gcc also allows one to specify the message printed
   when a deprecated function is used.  clang claims to be gcc 4.2, but
   may also support this feature.  */


#define __attribute_deprecated_msg__(msg) __attribute__ ((__deprecated__ (msg)))





/* At some point during the gcc 2.8 development the `format_arg' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.
   If several `format_arg' attributes are given for the same function, in
   gcc-3.0 and older, all but the last one are ignored.  In newer gccs,
   all designated arguments are considered.  */

#define __attribute_format_arg__(x) __attribute__ ((__format_arg__ (x)))




/* At some point during the gcc 2.97 development the `strfmon' format
   attribute for functions was introduced.  We don't want to use it
   unconditionally (although this would be possible) since it
   generates warnings.  */

#define __attribute_format_strfmon__(a,b) __attribute__ ((__format__ (__strfmon__, a, b)))





/* The nonull function attribute allows to mark pointer parameters which
   must not be NULL.  */

#define __nonnull(params) __attribute__ ((__nonnull__ params))




/* If fortification mode, we warn about unused results of certain
   function calls which can lead to problems.  */

#define __attribute_warn_unused_result__ __attribute__ ((__warn_unused_result__))








#define __wur 


/* Forces a function to be always inlined.  */

/* The Linux kernel defines __always_inline in stddef.h (283d7573), and
   it conflicts with this definition.  Therefore undefine it first to
   allow either header to be included first.  */
#undef __always_inline
#define __always_inline __inline __attribute__ ((__always_inline__))





/* Associate error messages with the source location of the call site rather
   than with the source location inside the function.  */

#define __attribute_artificial__ __attribute__ ((__artificial__))




/* GCC 4.3 and above with -std=c99 or -std=gnu99 implements ISO C99
   inline semantics, unless -fgnu89-inline is used.  Using __GNUC_STDC_INLINE__
   or __GNUC_GNU_INLINE is not a good enough check for gcc because gcc versions
   older than 4.3 may define these macros and still not guarantee GNU inlining
   semantics.

   clang++ identifies itself as gcc-4.2, but has support for GNU inlining
   semantics, that can be checked fot by using the __GNUC_STDC_INLINE_ and
   __GNUC_GNU_INLINE__ macro definitions.  */




#define __extern_inline extern __inline __attribute__ ((__gnu_inline__))
#define __extern_always_inline extern __always_inline __attribute__ ((__gnu_inline__))








#define __fortify_function __extern_always_inline __attribute_artificial__


/* GCC 4.3 and above allow passing all anonymous arguments of an
   __extern_always_inline function to some other vararg function.  */

#define __va_arg_pack() __builtin_va_arg_pack ()
#define __va_arg_pack_len() __builtin_va_arg_pack_len ()


/* It is possible to compile containing GCC extensions even if GCC is
   run in pedantic mode if the uses are carefully marked using the
   `__extension__' keyword.  But this is not generally available before
   version 2.8.  */




/* __restrict is known in EGCS 1.2 and above. */








/* ISO C99 also allows to declare arrays as non-overlapping.  The syntax is
     array_name[restrict]
   GCC 3.1 supports this.  */




#define __restrict_arr 



#define __glibc_unlikely(cond) __builtin_expect ((cond), 0)
#define __glibc_likely(cond) __builtin_expect ((cond), 1)






#define __glibc_has_attribute(attr) __has_attribute (attr)






/* Describes a char array whose address can safely be passed as the first
   argument to strncpy and strncat, as the char array is not necessarily
   a NUL-terminated string.  */
#define __attribute_nonstring__ __attribute__ ((__nonstring__))












/* Determine the wordsize from the preprocessor defines.  */


#define __WORDSIZE 64







#define __WORDSIZE_TIME64_COMPAT32 1
/* Both x86-64 and x32 use the 64-bit system call interface.  */
#define __SYSCALL_WORDSIZE 64
/* Properties of long double type.  ldbl-96 version.
   Copyright (C) 2016-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License  published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* long double is distinct from double, so there is nothing to
   define here.  */


#define __LDBL_REDIR1(name,proto,alias) name proto
#define __LDBL_REDIR(name,proto) name proto
#define __LDBL_REDIR1_NTH(name,proto,alias) name proto __THROW
#define __LDBL_REDIR_NTH(name,proto) name proto __THROW
#define __LDBL_REDIR_DECL(name) 

#define __REDIRECT_LDBL(name,proto,alias) __REDIRECT (name, proto, alias)
#define __REDIRECT_NTH_LDBL(name,proto,alias) __REDIRECT_NTH (name, proto, alias)




/* __glibc_macro_warning (MESSAGE) issues warning MESSAGE.  This is
   intended for use in preprocessor macros.

   Note: MESSAGE must be a _single_ string; concatenation of string
   literals is not supported.  */

#define __glibc_macro_warning1(message) _Pragma (#message)
#define __glibc_macro_warning(message) __glibc_macro_warning1 (GCC warning message)





/* Generic selection (ISO C11) is a C-only feature, available in GCC
   since version 4.9.  Previous versions do not provide generic
   selection, even though they might set __STDC_VERSION__ to 201112L,
   when in -std=c11 mode.  Thus, we must check for !defined __GNUC__
   when testing __STDC_VERSION__ for generic selection support.
   On the other hand, Clang also defines __GNUC__, so a clang-specific
   check is required to enable the use of generic selection.  */







#define __HAVE_GENERIC_SELECTION 0




/* If we don't have __REDIRECT, prototypes will be missing if
   __USE_FILE_OFFSET64 but not __USE_LARGEFILE[64]. */







/* Decide whether we can define 'extern inline' functions in headers.  */







/* This is here only because every header file already includes this one.
   Get the definitions of all the appropriate `__stub_FUNCTION' symbols.
   <gnu/stubs.h> contains `#define __stub_FUNCTION' when FUNCTION is a stub
   that will always return failure (and set errno to ENOSYS).  */
/* This file is automatically generated.
   This file selects the right generated file of `__stub_FUNCTION' macros
   based on the architecture being compiled for.  */






/* This file is automatically generated.
   It defines a symbol `__stub_FUNCTION' for each function
   in the C library which is a stub, meaning it will fail
   every time called, usually setting errno to ENOSYS.  */





#define __stub___compat_bdflush 
#define __stub_chflags 
#define __stub_fattach 
#define __stub_fchflags 
#define __stub_fdetach 
#define __stub_getmsg 
#define __stub_gtty 
#define __stub_lchmod 
#define __stub_putmsg 
#define __stub_revoke 
#define __stub_setlogin 
#define __stub_sigreturn 
#define __stub_sstk 
#define __stub_stty 




// Provide a declaration for the possibly deprecated gets function, as
// glibc 2.15 and later does not declare gets for ISO C11 when
// __GNU_SOURCE is defined.

#undef _GLIBCXX_HAVE_GETS


// Glibc 2.23 removed the obsolete isinf and isnan declarations. Check the
// version dynamically in case it has changed since libstdc++ was configured.
#define _GLIBCXX_NO_OBSOLETE_ISINF_ISNAN_DYNAMIC __GLIBC_PREREQ(2,23)


// Since glibc 2.27 pthread_self() is usable without linking to libpthread.
#define _GLIBCXX_NATIVE_THREAD_ID pthread_self()


// Pick up any CPU-specific definitions.
// Specific definitions for generic platforms  -*- C++ -*-

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/cpu_defines.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iosfwd}
 */


#define _GLIBCXX_CPU_DEFINES 1


// If platform uses neither visibility nor psuedo-visibility,
// specify empty default for namespace annotation macros.

#define _GLIBCXX_PSEUDO_VISIBILITY(V) 


// Certain function definitions that are meant to be overridable from
// user code are decorated with this macro.  For some targets, this
// macro causes these definitions to be weak.

#define _GLIBCXX_WEAK_DEFINITION 


// By default, we assume that __GXX_WEAK__ also means that there is support
// for declaring functions as weak while not defining such functions.  This
// allows for referring to functions provided by other libraries (e.g.,
// libitm) without depending on them if the respective features are not used.

#define _GLIBCXX_USE_WEAK_REF __GXX_WEAK__


// Conditionally enable annotations for the Transactional Memory TS on C++11.
// Most of the following conditions are due to limitations in the current
// implementation.







#define _GLIBCXX_TXN_SAFE 
#define _GLIBCXX_TXN_SAFE_DYN 



// In C++17 mathematical special functions are in namespace std.
#define _GLIBCXX_USE_STD_SPEC_FUNCS 1





// The remainder of the prewritten config is automatic; all the
// user hooks are listed above.

// Create a boolean flag to be used to determine if --fast-math is set.



#define _GLIBCXX_FAST_MATH 0


// This marks string literals in header files to be extracted for eventual
// translation.  It is primarily used for messages in thrown exceptions; see
// src/functexcept.cc.  We use __N because the more traditional _N is used
// for something else under certain OSes (see BADNAMES).
#define __N(msgid) (msgid)

// For example, <windows.h> is known to #define min and max as macros...
#undef min
#undef max

// N.B. these _GLIBCXX_USE_C99_XXX macros are defined unconditionally
// so they should be tested with #if not with #ifdef.


#define _GLIBCXX_USE_C99_MATH _GLIBCXX11_USE_C99_MATH


#define _GLIBCXX_USE_C99_COMPLEX _GLIBCXX11_USE_C99_COMPLEX


#define _GLIBCXX_USE_C99_STDIO _GLIBCXX11_USE_C99_STDIO


#define _GLIBCXX_USE_C99_STDLIB _GLIBCXX11_USE_C99_STDLIB


#define _GLIBCXX_USE_C99_WCHAR _GLIBCXX11_USE_C99_WCHAR


// Unless explicitly specified, enable char8_t extensions only if the core
// language char8_t feature macro is defined.









/* Define if __float128 is supported on this host.  */

/* For powerpc64 don't use __float128 when it's the same type as long double. */

#define _GLIBCXX_USE_FLOAT128 1



// Define if float has the IEEE binary32 format.



#define _GLIBCXX_FLOAT_IS_IEEE_BINARY32 1


// Define if double has the IEEE binary64 format.



#define _GLIBCXX_DOUBLE_IS_IEEE_BINARY64 1







#define _GLIBCXX_HAS_BUILTIN(B) __has_builtin(B)




#define _GLIBCXX_HAVE_BUILTIN_HAS_UNIQ_OBJ_REP 1



#define _GLIBCXX_HAVE_BUILTIN_IS_AGGREGATE 1



#define _GLIBCXX_HAVE_BUILTIN_IS_CONSTANT_EVALUATED 1



#define _GLIBCXX_HAVE_BUILTIN_IS_SAME 1



#define _GLIBCXX_HAVE_BUILTIN_LAUNDER 1


#undef _GLIBCXX_HAS_BUILTIN


#define __glibcxx_assert_1(_Condition) if (__builtin_is_constant_evaluated()) { void __failed_assertion(); if (!bool(_Condition)) __failed_assertion(); } else











#define __glibcxx_assert(_Condition) do { __glibcxx_assert_1(_Condition) { __glibcxx_assert_2(_Condition); } } while (false)






// PSTL configuration


// This header is not installed for freestanding:

// Preserved here so we have some idea which version of upstream we've pulled in
// #define PSTL_VERSION 9000

// For now this defaults to being based on the presence of Thread Building Blocks

#define _GLIBCXX_USE_TBB_PAR_BACKEND __has_include(<tbb/tbb.h>)

// This section will need some rework when a new (default) backend type is added



#define _PSTL_PAR_BACKEND_SERIAL 


#define _PSTL_ASSERT(_Condition) __glibcxx_assert(_Condition)
#define _PSTL_ASSERT_MSG(_Condition,_Message) __glibcxx_assert(_Condition)

// -*- C++ -*-
//===-- pstl_config.h -----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//


#define _PSTL_CONFIG_H 

// The version is XYYZ, where X is major, YY is minor, and Z is patch (i.e. X.YY.Z)
#define _PSTL_VERSION 12000
#define _PSTL_VERSION_MAJOR (_PSTL_VERSION / 1000)
#define _PSTL_VERSION_MINOR ((_PSTL_VERSION % 1000) / 10)
#define _PSTL_VERSION_PATCH (_PSTL_VERSION % 10)





// Check the user-defined macro for warnings





#define _PSTL_USAGE_WARNINGS 0


// Portability "#pragma" definition



#define _PSTL_PRAGMA(x) _Pragma(#x)


#define _PSTL_STRING_AUX(x) #x
#define _PSTL_STRING(x) _PSTL_STRING_AUX(x)
#define _PSTL_STRING_CONCAT(x,y) x #y






#define _PSTL_HIDE_FROM_ABI_PUSH 
#define _PSTL_HIDE_FROM_ABI_POP 


// note that when ICC or Clang is in use, _PSTL_GCC_VERSION might not fully match
// the actual GCC version on the system.
#define _PSTL_GCC_VERSION (__GNUC__ * 10000 + __GNUC_MINOR__ * 100 + __GNUC_PATCHLEVEL__)






// Enable SIMD for compilers that support OpenMP 4.0


#define _PSTL_PRAGMA_SIMD _PSTL_PRAGMA(omp simd)
#define _PSTL_PRAGMA_DECLARE_SIMD _PSTL_PRAGMA(omp declare simd)
#define _PSTL_PRAGMA_SIMD_REDUCTION(PRM) _PSTL_PRAGMA(omp simd reduction(PRM))




#define _PSTL_PRAGMA_FORCEINLINE 







#define _PSTL_PRAGMA_SIMD_SCAN(PRM) 
#define _PSTL_PRAGMA_SIMD_INCLUSIVE_SCAN(PRM) 
#define _PSTL_PRAGMA_SIMD_EXCLUSIVE_SCAN(PRM) 


// Should be defined to 1 for environments with a vendor implementation of C++17 execution policies
#define _PSTL_CPP17_EXECUTION_POLICIES_PRESENT (_MSC_VER >= 1912)

#define _PSTL_CPP14_2RANGE_MISMATCH_EQUAL_PRESENT (_MSC_VER >= 1900 || __cplusplus >= 201300L || __cpp_lib_robust_nonmodifying_seq_ops == 201304)

#define _PSTL_CPP14_MAKE_REVERSE_ITERATOR_PRESENT (_MSC_VER >= 1900 || __cplusplus >= 201402L || __cpp_lib_make_reverse_iterator == 201402)

#define _PSTL_CPP14_INTEGER_SEQUENCE_PRESENT (_MSC_VER >= 1900 || __cplusplus >= 201402L)
#define _PSTL_CPP14_VARIABLE_TEMPLATES_PRESENT (!__INTEL_COMPILER || __INTEL_COMPILER >= 1700) && (_MSC_FULL_VER >= 190023918 || __cplusplus >= 201402L)


#define _PSTL_EARLYEXIT_PRESENT (__INTEL_COMPILER >= 1800)
#define _PSTL_MONOTONIC_PRESENT (__INTEL_COMPILER >= 1800)


#define _PSTL_UDR_PRESENT 1




#define _PSTL_UDS_PRESENT (__INTEL_COMPILER >= 1900 && __INTEL_COMPILER_BUILD_DATE >= 20180626)




#define _PSTL_PRAGMA_SIMD_EARLYEXIT 






#define _PSTL_PRAGMA_SIMD_ORDERED_MONOTONIC(PRM) 
#define _PSTL_PRAGMA_SIMD_ORDERED_MONOTONIC_2ARGS(PRM1,PRM2) 


// Declaration of reduction functor, where
// NAME - the name of the functor
// OP - type of the callable object with the reduction operation
// omp_in - refers to the local partial result
// omp_out - refers to the final value of the combiner operator
// omp_priv - refers to the private copy of the initial value
// omp_orig - refers to the original variable to be reduced
#define _PSTL_PRAGMA_DECLARE_REDUCTION(NAME,OP) _PSTL_PRAGMA(omp declare reduction(NAME:OP : omp_out(omp_in)) initializer(omp_priv = omp_orig))





#define _PSTL_PRAGMA_VECTOR_UNALIGNED 


// Check the user-defined macro to use non-temporal stores



#define _PSTL_USE_NONTEMPORAL_STORES_IF_ALLOWED 





#define _PSTL_PRAGMA_LOCATION " [Parallel STL message]: "


#define _PSTL_PRAGMA_MESSAGE_IMPL(x) _PSTL_PRAGMA(message(_PSTL_STRING_CONCAT(_PSTL_PRAGMA_LOCATION, x)))





#define _PSTL_PRAGMA_MESSAGE(x) 
#define _PSTL_PRAGMA_MESSAGE_POLICIES(x) 


// broken macros
#define _PSTL_CPP11_STD_ROTATE_BROKEN ((__GLIBCXX__ && __GLIBCXX__ < 20150716) || (_MSC_VER && _MSC_VER < 1800))

#define _PSTL_ICC_18_OMP_SIMD_BROKEN (__INTEL_COMPILER == 1800)




// End of prewritten config; the settings discovered at configure time follow.
/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Define to 1 if you have the `acosf' function. */
#define _GLIBCXX_HAVE_ACOSF 1

/* Define to 1 if you have the `acosl' function. */
#define _GLIBCXX_HAVE_ACOSL 1

/* Define to 1 if you have the `aligned_alloc' function. */
#define _GLIBCXX_HAVE_ALIGNED_ALLOC 1

/* Define to 1 if you have the <arpa/inet.h> header file. */
#define _GLIBCXX_HAVE_ARPA_INET_H 1

/* Define to 1 if you have the `asinf' function. */
#define _GLIBCXX_HAVE_ASINF 1

/* Define to 1 if you have the `asinl' function. */
#define _GLIBCXX_HAVE_ASINL 1

/* Define to 1 if the target assembler supports .symver directive. */
#define _GLIBCXX_HAVE_AS_SYMVER_DIRECTIVE 1

/* Define to 1 if you have the `atan2f' function. */
#define _GLIBCXX_HAVE_ATAN2F 1

/* Define to 1 if you have the `atan2l' function. */
#define _GLIBCXX_HAVE_ATAN2L 1

/* Define to 1 if you have the `atanf' function. */
#define _GLIBCXX_HAVE_ATANF 1

/* Define to 1 if you have the `atanl' function. */
#define _GLIBCXX_HAVE_ATANL 1

/* Defined if shared_ptr reference counting should use atomic operations. */
#define _GLIBCXX_HAVE_ATOMIC_LOCK_POLICY 1

/* Define to 1 if you have the `at_quick_exit' function. */
#define _GLIBCXX_HAVE_AT_QUICK_EXIT 1

/* Define to 1 if the target assembler supports thread-local storage. */
/* #undef _GLIBCXX_HAVE_CC_TLS */

/* Define to 1 if you have the `ceilf' function. */
#define _GLIBCXX_HAVE_CEILF 1

/* Define to 1 if you have the `ceill' function. */
#define _GLIBCXX_HAVE_CEILL 1

/* Define to 1 if you have the <complex.h> header file. */
#define _GLIBCXX_HAVE_COMPLEX_H 1

/* Define to 1 if you have the `cosf' function. */
#define _GLIBCXX_HAVE_COSF 1

/* Define to 1 if you have the `coshf' function. */
#define _GLIBCXX_HAVE_COSHF 1

/* Define to 1 if you have the `coshl' function. */
#define _GLIBCXX_HAVE_COSHL 1

/* Define to 1 if you have the `cosl' function. */
#define _GLIBCXX_HAVE_COSL 1

/* Define to 1 if you have the <dirent.h> header file. */
#define _GLIBCXX_HAVE_DIRENT_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define _GLIBCXX_HAVE_DLFCN_H 1

/* Define to 1 if you have the <endian.h> header file. */
#define _GLIBCXX_HAVE_ENDIAN_H 1

/* Define to 1 if GCC 4.6 supported std::exception_ptr for the target */
#define _GLIBCXX_HAVE_EXCEPTION_PTR_SINCE_GCC46 1

/* Define to 1 if you have the <execinfo.h> header file. */
#define _GLIBCXX_HAVE_EXECINFO_H 1

/* Define to 1 if you have the `expf' function. */
#define _GLIBCXX_HAVE_EXPF 1

/* Define to 1 if you have the `expl' function. */
#define _GLIBCXX_HAVE_EXPL 1

/* Define to 1 if you have the `fabsf' function. */
#define _GLIBCXX_HAVE_FABSF 1

/* Define to 1 if you have the `fabsl' function. */
#define _GLIBCXX_HAVE_FABSL 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define _GLIBCXX_HAVE_FCNTL_H 1

/* Define to 1 if you have the <fenv.h> header file. */
#define _GLIBCXX_HAVE_FENV_H 1

/* Define to 1 if you have the `finite' function. */
#define _GLIBCXX_HAVE_FINITE 1

/* Define to 1 if you have the `finitef' function. */
#define _GLIBCXX_HAVE_FINITEF 1

/* Define to 1 if you have the `finitel' function. */
#define _GLIBCXX_HAVE_FINITEL 1

/* Define to 1 if you have the <float.h> header file. */
#define _GLIBCXX_HAVE_FLOAT_H 1

/* Define to 1 if you have the `floorf' function. */
#define _GLIBCXX_HAVE_FLOORF 1

/* Define to 1 if you have the `floorl' function. */
#define _GLIBCXX_HAVE_FLOORL 1

/* Define to 1 if you have the `fmodf' function. */
#define _GLIBCXX_HAVE_FMODF 1

/* Define to 1 if you have the `fmodl' function. */
#define _GLIBCXX_HAVE_FMODL 1

/* Define to 1 if you have the `fpclass' function. */
/* #undef _GLIBCXX_HAVE_FPCLASS */

/* Define to 1 if you have the <fp.h> header file. */
/* #undef _GLIBCXX_HAVE_FP_H */

/* Define to 1 if you have the `frexpf' function. */
#define _GLIBCXX_HAVE_FREXPF 1

/* Define to 1 if you have the `frexpl' function. */
#define _GLIBCXX_HAVE_FREXPL 1

/* Define if _Unwind_GetIPInfo is available. */
#define _GLIBCXX_HAVE_GETIPINFO 1

/* Define if gets is available in <stdio.h> before C++14. */
#define _GLIBCXX_HAVE_GETS 1

/* Define to 1 if you have the `hypot' function. */
#define _GLIBCXX_HAVE_HYPOT 1

/* Define to 1 if you have the `hypotf' function. */
#define _GLIBCXX_HAVE_HYPOTF 1

/* Define to 1 if you have the `hypotl' function. */
#define _GLIBCXX_HAVE_HYPOTL 1

/* Define if you have the iconv() function. */
#define _GLIBCXX_HAVE_ICONV 1

/* Define to 1 if you have the <ieeefp.h> header file. */
/* #undef _GLIBCXX_HAVE_IEEEFP_H */

/* Define if int64_t is available in <stdint.h>. */
#define _GLIBCXX_HAVE_INT64_T 1

/* Define if int64_t is a long. */
#define _GLIBCXX_HAVE_INT64_T_LONG 1

/* Define if int64_t is a long long. */
/* #undef _GLIBCXX_HAVE_INT64_T_LONG_LONG */

/* Define to 1 if you have the <inttypes.h> header file. */
#define _GLIBCXX_HAVE_INTTYPES_H 1

/* Define to 1 if you have the `isinf' function. */
/* #undef _GLIBCXX_HAVE_ISINF */

/* Define to 1 if you have the `isinff' function. */
#define _GLIBCXX_HAVE_ISINFF 1

/* Define to 1 if you have the `isinfl' function. */
#define _GLIBCXX_HAVE_ISINFL 1

/* Define to 1 if you have the `isnan' function. */
/* #undef _GLIBCXX_HAVE_ISNAN */

/* Define to 1 if you have the `isnanf' function. */
#define _GLIBCXX_HAVE_ISNANF 1

/* Define to 1 if you have the `isnanl' function. */
#define _GLIBCXX_HAVE_ISNANL 1

/* Defined if iswblank exists. */
#define _GLIBCXX_HAVE_ISWBLANK 1

/* Define if LC_MESSAGES is available in <locale.h>. */
#define _GLIBCXX_HAVE_LC_MESSAGES 1

/* Define to 1 if you have the `ldexpf' function. */
#define _GLIBCXX_HAVE_LDEXPF 1

/* Define to 1 if you have the `ldexpl' function. */
#define _GLIBCXX_HAVE_LDEXPL 1

/* Define to 1 if you have the <libintl.h> header file. */
#define _GLIBCXX_HAVE_LIBINTL_H 1

/* Only used in build directory testsuite_hooks.h. */
#define _GLIBCXX_HAVE_LIMIT_AS 1

/* Only used in build directory testsuite_hooks.h. */
#define _GLIBCXX_HAVE_LIMIT_DATA 1

/* Only used in build directory testsuite_hooks.h. */
#define _GLIBCXX_HAVE_LIMIT_FSIZE 1

/* Only used in build directory testsuite_hooks.h. */
#define _GLIBCXX_HAVE_LIMIT_RSS 1

/* Only used in build directory testsuite_hooks.h. */
#define _GLIBCXX_HAVE_LIMIT_VMEM 0

/* Define if link is available in <unistd.h>. */
#define _GLIBCXX_HAVE_LINK 1

/* Define if futex syscall is available. */
#define _GLIBCXX_HAVE_LINUX_FUTEX 1

/* Define to 1 if you have the <linux/random.h> header file. */
#define _GLIBCXX_HAVE_LINUX_RANDOM_H 1

/* Define to 1 if you have the <linux/types.h> header file. */
#define _GLIBCXX_HAVE_LINUX_TYPES_H 1

/* Define to 1 if you have the <locale.h> header file. */
#define _GLIBCXX_HAVE_LOCALE_H 1

/* Define to 1 if you have the `log10f' function. */
#define _GLIBCXX_HAVE_LOG10F 1

/* Define to 1 if you have the `log10l' function. */
#define _GLIBCXX_HAVE_LOG10L 1

/* Define to 1 if you have the `logf' function. */
#define _GLIBCXX_HAVE_LOGF 1

/* Define to 1 if you have the `logl' function. */
#define _GLIBCXX_HAVE_LOGL 1

/* Define to 1 if you have the <machine/endian.h> header file. */
/* #undef _GLIBCXX_HAVE_MACHINE_ENDIAN_H */

/* Define to 1 if you have the <machine/param.h> header file. */
/* #undef _GLIBCXX_HAVE_MACHINE_PARAM_H */

/* Define if mbstate_t exists in wchar.h. */
#define _GLIBCXX_HAVE_MBSTATE_T 1

/* Define to 1 if you have the `memalign' function. */
#define _GLIBCXX_HAVE_MEMALIGN 1

/* Define to 1 if you have the <memory.h> header file. */
#define _GLIBCXX_HAVE_MEMORY_H 1

/* Define to 1 if you have the `modf' function. */
#define _GLIBCXX_HAVE_MODF 1

/* Define to 1 if you have the `modff' function. */
#define _GLIBCXX_HAVE_MODFF 1

/* Define to 1 if you have the `modfl' function. */
#define _GLIBCXX_HAVE_MODFL 1

/* Define to 1 if you have the <nan.h> header file. */
/* #undef _GLIBCXX_HAVE_NAN_H */

/* Define to 1 if you have the <netdb.h> header file. */
#define _GLIBCXX_HAVE_NETDB_H 1

/* Define to 1 if you have the <netinet/in.h> header file. */
#define _GLIBCXX_HAVE_NETINET_IN_H 1

/* Define to 1 if you have the <netinet/tcp.h> header file. */
#define _GLIBCXX_HAVE_NETINET_TCP_H 1

/* Define if <math.h> defines obsolete isinf function. */
/* #undef _GLIBCXX_HAVE_OBSOLETE_ISINF */

/* Define if <math.h> defines obsolete isnan function. */
/* #undef _GLIBCXX_HAVE_OBSOLETE_ISNAN */

/* Define if poll is available in <poll.h>. */
#define _GLIBCXX_HAVE_POLL 1

/* Define to 1 if you have the <poll.h> header file. */
#define _GLIBCXX_HAVE_POLL_H 1

/* Define to 1 if you have the `posix_memalign' function. */
#define _GLIBCXX_HAVE_POSIX_MEMALIGN 1

/* Define to 1 if you have the `powf' function. */
#define _GLIBCXX_HAVE_POWF 1

/* Define to 1 if you have the `powl' function. */
#define _GLIBCXX_HAVE_POWL 1

/* Define to 1 if you have the `qfpclass' function. */
/* #undef _GLIBCXX_HAVE_QFPCLASS */

/* Define to 1 if you have the `quick_exit' function. */
#define _GLIBCXX_HAVE_QUICK_EXIT 1

/* Define if readlink is available in <unistd.h>. */
#define _GLIBCXX_HAVE_READLINK 1

/* Define to 1 if you have the `setenv' function. */
#define _GLIBCXX_HAVE_SETENV 1

/* Define to 1 if you have the `sincos' function. */
#define _GLIBCXX_HAVE_SINCOS 1

/* Define to 1 if you have the `sincosf' function. */
#define _GLIBCXX_HAVE_SINCOSF 1

/* Define to 1 if you have the `sincosl' function. */
#define _GLIBCXX_HAVE_SINCOSL 1

/* Define to 1 if you have the `sinf' function. */
#define _GLIBCXX_HAVE_SINF 1

/* Define to 1 if you have the `sinhf' function. */
#define _GLIBCXX_HAVE_SINHF 1

/* Define to 1 if you have the `sinhl' function. */
#define _GLIBCXX_HAVE_SINHL 1

/* Define to 1 if you have the `sinl' function. */
#define _GLIBCXX_HAVE_SINL 1

/* Defined if sleep exists. */
/* #undef _GLIBCXX_HAVE_SLEEP */

/* Define to 1 if you have the `sockatmark' function. */
#define _GLIBCXX_HAVE_SOCKATMARK 1

/* Define to 1 if you have the `sqrtf' function. */
#define _GLIBCXX_HAVE_SQRTF 1

/* Define to 1 if you have the `sqrtl' function. */
#define _GLIBCXX_HAVE_SQRTL 1

/* Define to 1 if you have the <stdalign.h> header file. */
#define _GLIBCXX_HAVE_STDALIGN_H 1

/* Define to 1 if you have the <stdbool.h> header file. */
#define _GLIBCXX_HAVE_STDBOOL_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define _GLIBCXX_HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define _GLIBCXX_HAVE_STDLIB_H 1

/* Define if strerror_l is available in <string.h>. */
#define _GLIBCXX_HAVE_STRERROR_L 1

/* Define if strerror_r is available in <string.h>. */
#define _GLIBCXX_HAVE_STRERROR_R 1

/* Define to 1 if you have the <strings.h> header file. */
#define _GLIBCXX_HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define _GLIBCXX_HAVE_STRING_H 1

/* Define to 1 if you have the `strtof' function. */
#define _GLIBCXX_HAVE_STRTOF 1

/* Define to 1 if you have the `strtold' function. */
#define _GLIBCXX_HAVE_STRTOLD 1

/* Define to 1 if `d_type' is a member of `struct dirent'. */
#define _GLIBCXX_HAVE_STRUCT_DIRENT_D_TYPE 1

/* Define if strxfrm_l is available in <string.h>. */
#define _GLIBCXX_HAVE_STRXFRM_L 1

/* Define if symlink is available in <unistd.h>. */
#define _GLIBCXX_HAVE_SYMLINK 1

/* Define to 1 if the target runtime linker supports binding the same symbol
   to different versions. */
#define _GLIBCXX_HAVE_SYMVER_SYMBOL_RENAMING_RUNTIME_SUPPORT 1

/* Define to 1 if you have the <sys/filio.h> header file. */
/* #undef _GLIBCXX_HAVE_SYS_FILIO_H */

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define _GLIBCXX_HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/ipc.h> header file. */
#define _GLIBCXX_HAVE_SYS_IPC_H 1

/* Define to 1 if you have the <sys/isa_defs.h> header file. */
/* #undef _GLIBCXX_HAVE_SYS_ISA_DEFS_H */

/* Define to 1 if you have the <sys/machine.h> header file. */
/* #undef _GLIBCXX_HAVE_SYS_MACHINE_H */

/* Define to 1 if you have the <sys/param.h> header file. */
#define _GLIBCXX_HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/resource.h> header file. */
#define _GLIBCXX_HAVE_SYS_RESOURCE_H 1

/* Define to 1 if you have a suitable <sys/sdt.h> header file */
#define _GLIBCXX_HAVE_SYS_SDT_H 1

/* Define to 1 if you have the <sys/sem.h> header file. */
#define _GLIBCXX_HAVE_SYS_SEM_H 1

/* Define to 1 if you have the <sys/socket.h> header file. */
#define _GLIBCXX_HAVE_SYS_SOCKET_H 1

/* Define to 1 if you have the <sys/statvfs.h> header file. */
#define _GLIBCXX_HAVE_SYS_STATVFS_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define _GLIBCXX_HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/sysinfo.h> header file. */
#define _GLIBCXX_HAVE_SYS_SYSINFO_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define _GLIBCXX_HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define _GLIBCXX_HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/uio.h> header file. */
#define _GLIBCXX_HAVE_SYS_UIO_H 1

/* Define if S_IFREG is available in <sys/stat.h>. */
/* #undef _GLIBCXX_HAVE_S_IFREG */

/* Define if S_ISREG is available in <sys/stat.h>. */
#define _GLIBCXX_HAVE_S_ISREG 1

/* Define to 1 if you have the `tanf' function. */
#define _GLIBCXX_HAVE_TANF 1

/* Define to 1 if you have the `tanhf' function. */
#define _GLIBCXX_HAVE_TANHF 1

/* Define to 1 if you have the `tanhl' function. */
#define _GLIBCXX_HAVE_TANHL 1

/* Define to 1 if you have the `tanl' function. */
#define _GLIBCXX_HAVE_TANL 1

/* Define to 1 if you have the <tgmath.h> header file. */
#define _GLIBCXX_HAVE_TGMATH_H 1

/* Define to 1 if you have the `timespec_get' function. */
#define _GLIBCXX_HAVE_TIMESPEC_GET 1

/* Define to 1 if the target supports thread-local storage. */
#define _GLIBCXX_HAVE_TLS 1

/* Define if truncate is available in <unistd.h>. */
#define _GLIBCXX_HAVE_TRUNCATE 1

/* Define to 1 if you have the <uchar.h> header file. */
#define _GLIBCXX_HAVE_UCHAR_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define _GLIBCXX_HAVE_UNISTD_H 1

/* Define to 1 if you have the `uselocale' function. */
#define _GLIBCXX_HAVE_USELOCALE 1

/* Defined if usleep exists. */
/* #undef _GLIBCXX_HAVE_USLEEP */

/* Define to 1 if you have the <utime.h> header file. */
#define _GLIBCXX_HAVE_UTIME_H 1

/* Defined if vfwscanf exists. */
#define _GLIBCXX_HAVE_VFWSCANF 1

/* Defined if vswscanf exists. */
#define _GLIBCXX_HAVE_VSWSCANF 1

/* Defined if vwscanf exists. */
#define _GLIBCXX_HAVE_VWSCANF 1

/* Define to 1 if you have the <wchar.h> header file. */
#define _GLIBCXX_HAVE_WCHAR_H 1

/* Defined if wcstof exists. */
#define _GLIBCXX_HAVE_WCSTOF 1

/* Define to 1 if you have the <wctype.h> header file. */
#define _GLIBCXX_HAVE_WCTYPE_H 1

/* Defined if Sleep exists. */
/* #undef _GLIBCXX_HAVE_WIN32_SLEEP */

/* Define if writev is available in <sys/uio.h>. */
#define _GLIBCXX_HAVE_WRITEV 1

/* Define to 1 if you have the <xlocale.h> header file. */
/* #undef _GLIBCXX_HAVE_XLOCALE_H */

/* Define to 1 if you have the `_acosf' function. */
/* #undef _GLIBCXX_HAVE__ACOSF */

/* Define to 1 if you have the `_acosl' function. */
/* #undef _GLIBCXX_HAVE__ACOSL */

/* Define to 1 if you have the `_aligned_malloc' function. */
/* #undef _GLIBCXX_HAVE__ALIGNED_MALLOC */

/* Define to 1 if you have the `_asinf' function. */
/* #undef _GLIBCXX_HAVE__ASINF */

/* Define to 1 if you have the `_asinl' function. */
/* #undef _GLIBCXX_HAVE__ASINL */

/* Define to 1 if you have the `_atan2f' function. */
/* #undef _GLIBCXX_HAVE__ATAN2F */

/* Define to 1 if you have the `_atan2l' function. */
/* #undef _GLIBCXX_HAVE__ATAN2L */

/* Define to 1 if you have the `_atanf' function. */
/* #undef _GLIBCXX_HAVE__ATANF */

/* Define to 1 if you have the `_atanl' function. */
/* #undef _GLIBCXX_HAVE__ATANL */

/* Define to 1 if you have the `_ceilf' function. */
/* #undef _GLIBCXX_HAVE__CEILF */

/* Define to 1 if you have the `_ceill' function. */
/* #undef _GLIBCXX_HAVE__CEILL */

/* Define to 1 if you have the `_cosf' function. */
/* #undef _GLIBCXX_HAVE__COSF */

/* Define to 1 if you have the `_coshf' function. */
/* #undef _GLIBCXX_HAVE__COSHF */

/* Define to 1 if you have the `_coshl' function. */
/* #undef _GLIBCXX_HAVE__COSHL */

/* Define to 1 if you have the `_cosl' function. */
/* #undef _GLIBCXX_HAVE__COSL */

/* Define to 1 if you have the `_expf' function. */
/* #undef _GLIBCXX_HAVE__EXPF */

/* Define to 1 if you have the `_expl' function. */
/* #undef _GLIBCXX_HAVE__EXPL */

/* Define to 1 if you have the `_fabsf' function. */
/* #undef _GLIBCXX_HAVE__FABSF */

/* Define to 1 if you have the `_fabsl' function. */
/* #undef _GLIBCXX_HAVE__FABSL */

/* Define to 1 if you have the `_finite' function. */
/* #undef _GLIBCXX_HAVE__FINITE */

/* Define to 1 if you have the `_finitef' function. */
/* #undef _GLIBCXX_HAVE__FINITEF */

/* Define to 1 if you have the `_finitel' function. */
/* #undef _GLIBCXX_HAVE__FINITEL */

/* Define to 1 if you have the `_floorf' function. */
/* #undef _GLIBCXX_HAVE__FLOORF */

/* Define to 1 if you have the `_floorl' function. */
/* #undef _GLIBCXX_HAVE__FLOORL */

/* Define to 1 if you have the `_fmodf' function. */
/* #undef _GLIBCXX_HAVE__FMODF */

/* Define to 1 if you have the `_fmodl' function. */
/* #undef _GLIBCXX_HAVE__FMODL */

/* Define to 1 if you have the `_fpclass' function. */
/* #undef _GLIBCXX_HAVE__FPCLASS */

/* Define to 1 if you have the `_frexpf' function. */
/* #undef _GLIBCXX_HAVE__FREXPF */

/* Define to 1 if you have the `_frexpl' function. */
/* #undef _GLIBCXX_HAVE__FREXPL */

/* Define to 1 if you have the `_hypot' function. */
/* #undef _GLIBCXX_HAVE__HYPOT */

/* Define to 1 if you have the `_hypotf' function. */
/* #undef _GLIBCXX_HAVE__HYPOTF */

/* Define to 1 if you have the `_hypotl' function. */
/* #undef _GLIBCXX_HAVE__HYPOTL */

/* Define to 1 if you have the `_isinf' function. */
/* #undef _GLIBCXX_HAVE__ISINF */

/* Define to 1 if you have the `_isinff' function. */
/* #undef _GLIBCXX_HAVE__ISINFF */

/* Define to 1 if you have the `_isinfl' function. */
/* #undef _GLIBCXX_HAVE__ISINFL */

/* Define to 1 if you have the `_isnan' function. */
/* #undef _GLIBCXX_HAVE__ISNAN */

/* Define to 1 if you have the `_isnanf' function. */
/* #undef _GLIBCXX_HAVE__ISNANF */

/* Define to 1 if you have the `_isnanl' function. */
/* #undef _GLIBCXX_HAVE__ISNANL */

/* Define to 1 if you have the `_ldexpf' function. */
/* #undef _GLIBCXX_HAVE__LDEXPF */

/* Define to 1 if you have the `_ldexpl' function. */
/* #undef _GLIBCXX_HAVE__LDEXPL */

/* Define to 1 if you have the `_log10f' function. */
/* #undef _GLIBCXX_HAVE__LOG10F */

/* Define to 1 if you have the `_log10l' function. */
/* #undef _GLIBCXX_HAVE__LOG10L */

/* Define to 1 if you have the `_logf' function. */
/* #undef _GLIBCXX_HAVE__LOGF */

/* Define to 1 if you have the `_logl' function. */
/* #undef _GLIBCXX_HAVE__LOGL */

/* Define to 1 if you have the `_modf' function. */
/* #undef _GLIBCXX_HAVE__MODF */

/* Define to 1 if you have the `_modff' function. */
/* #undef _GLIBCXX_HAVE__MODFF */

/* Define to 1 if you have the `_modfl' function. */
/* #undef _GLIBCXX_HAVE__MODFL */

/* Define to 1 if you have the `_powf' function. */
/* #undef _GLIBCXX_HAVE__POWF */

/* Define to 1 if you have the `_powl' function. */
/* #undef _GLIBCXX_HAVE__POWL */

/* Define to 1 if you have the `_qfpclass' function. */
/* #undef _GLIBCXX_HAVE__QFPCLASS */

/* Define to 1 if you have the `_sincos' function. */
/* #undef _GLIBCXX_HAVE__SINCOS */

/* Define to 1 if you have the `_sincosf' function. */
/* #undef _GLIBCXX_HAVE__SINCOSF */

/* Define to 1 if you have the `_sincosl' function. */
/* #undef _GLIBCXX_HAVE__SINCOSL */

/* Define to 1 if you have the `_sinf' function. */
/* #undef _GLIBCXX_HAVE__SINF */

/* Define to 1 if you have the `_sinhf' function. */
/* #undef _GLIBCXX_HAVE__SINHF */

/* Define to 1 if you have the `_sinhl' function. */
/* #undef _GLIBCXX_HAVE__SINHL */

/* Define to 1 if you have the `_sinl' function. */
/* #undef _GLIBCXX_HAVE__SINL */

/* Define to 1 if you have the `_sqrtf' function. */
/* #undef _GLIBCXX_HAVE__SQRTF */

/* Define to 1 if you have the `_sqrtl' function. */
/* #undef _GLIBCXX_HAVE__SQRTL */

/* Define to 1 if you have the `_tanf' function. */
/* #undef _GLIBCXX_HAVE__TANF */

/* Define to 1 if you have the `_tanhf' function. */
/* #undef _GLIBCXX_HAVE__TANHF */

/* Define to 1 if you have the `_tanhl' function. */
/* #undef _GLIBCXX_HAVE__TANHL */

/* Define to 1 if you have the `_tanl' function. */
/* #undef _GLIBCXX_HAVE__TANL */

/* Define to 1 if you have the `_wfopen' function. */
/* #undef _GLIBCXX_HAVE__WFOPEN */

/* Define to 1 if you have the `__cxa_thread_atexit' function. */
/* #undef _GLIBCXX_HAVE___CXA_THREAD_ATEXIT */

/* Define to 1 if you have the `__cxa_thread_atexit_impl' function. */
#define _GLIBCXX_HAVE___CXA_THREAD_ATEXIT_IMPL 1

/* Define as const if the declaration of iconv() needs const. */
#define _GLIBCXX_ICONV_CONST 

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Defined if no way to sleep is available. */
/* #undef NO_SLEEP */

/* Name of package */
/* #undef _GLIBCXX_PACKAGE */

/* Define to the address where bug reports for this package should be sent. */
#define _GLIBCXX_PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define _GLIBCXX_PACKAGE_NAME "package-unused"

/* Define to the full name and version of this package. */
#define _GLIBCXX_PACKAGE_STRING "package-unused version-unused"

/* Define to the one symbol short name of this package. */
#define _GLIBCXX_PACKAGE_TARNAME "libstdc++"

/* Define to the home page for this package. */
#define _GLIBCXX_PACKAGE_URL ""

/* Define to the version of this package. */
#define _GLIBCXX_PACKAGE__GLIBCXX_VERSION "version-unused"

/* The size of `char', as computed by sizeof. */
/* #undef SIZEOF_CHAR */

/* The size of `int', as computed by sizeof. */
/* #undef SIZEOF_INT */

/* The size of `long', as computed by sizeof. */
/* #undef SIZEOF_LONG */

/* The size of `short', as computed by sizeof. */
/* #undef SIZEOF_SHORT */

/* The size of `void *', as computed by sizeof. */
/* #undef SIZEOF_VOID_P */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Version number of package */
/* #undef _GLIBCXX_VERSION */

/* Enable large inode numbers on Mac OS X 10.5.  */

#define _GLIBCXX_DARWIN_USE_64_BIT_INODE 1


/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef _GLIBCXX_FILE_OFFSET_BITS */

/* Define if C99 functions in <complex.h> should be used in <complex> for
   C++11. Using compiler builtins for these functions requires corresponding
   C99 library functions to be present. */
#define _GLIBCXX11_USE_C99_COMPLEX 1

/* Define if C99 functions or macros in <math.h> should be imported in <cmath>
   in namespace std for C++11. */
#define _GLIBCXX11_USE_C99_MATH 1

/* Define if C99 functions or macros in <stdio.h> should be imported in
   <cstdio> in namespace std for C++11. */
#define _GLIBCXX11_USE_C99_STDIO 1

/* Define if C99 functions or macros in <stdlib.h> should be imported in
   <cstdlib> in namespace std for C++11. */
#define _GLIBCXX11_USE_C99_STDLIB 1

/* Define if C99 functions or macros in <wchar.h> should be imported in
   <cwchar> in namespace std for C++11. */
#define _GLIBCXX11_USE_C99_WCHAR 1

/* Define if C99 functions in <complex.h> should be used in <complex> for
   C++98. Using compiler builtins for these functions requires corresponding
   C99 library functions to be present. */
#define _GLIBCXX98_USE_C99_COMPLEX 1

/* Define if C99 functions or macros in <math.h> should be imported in <cmath>
   in namespace std for C++98. */
#define _GLIBCXX98_USE_C99_MATH 1

/* Define if C99 functions or macros in <stdio.h> should be imported in
   <cstdio> in namespace std for C++98. */
#define _GLIBCXX98_USE_C99_STDIO 1

/* Define if C99 functions or macros in <stdlib.h> should be imported in
   <cstdlib> in namespace std for C++98. */
#define _GLIBCXX98_USE_C99_STDLIB 1

/* Define if C99 functions or macros in <wchar.h> should be imported in
   <cwchar> in namespace std for C++98. */
#define _GLIBCXX98_USE_C99_WCHAR 1

/* Define if the compiler supports C++11 atomics. */
#define _GLIBCXX_ATOMIC_BUILTINS 1

/* Define to use concept checking code from the boost libraries. */
/* #undef _GLIBCXX_CONCEPT_CHECKS */

/* Define to 1 if a fully dynamic basic_string is wanted, 0 to disable,
   undefined for platform defaults */
#define _GLIBCXX_FULLY_DYNAMIC_STRING 0

/* Define if gthreads library is available. */
#define _GLIBCXX_HAS_GTHREADS 1

/* Define to 1 if POSIX Semaphores with sem_timedwait are available in
   <semaphore.h>. */
#define _GLIBCXX__GLIBCXX_HAVE_POSIX_SEMAPHORE 1

/* Define to 1 if a full hosted library is built, or 0 if freestanding. */
#define _GLIBCXX_HOSTED 1

/* Define if compatibility should be provided for alternative 128-bit long
   double formats. */

/* Define if compatibility should be provided for -mlong-double-64. */

/* Define to the letter to which size_t is mangled. */
#define _GLIBCXX_MANGLE_SIZE_T m

/* Define if C99 llrint and llround functions are missing from <math.h>. */
/* #undef _GLIBCXX_NO_C99_ROUNDING_FUNCS */

/* Define if ptrdiff_t is int. */
/* #undef _GLIBCXX_PTRDIFF_T_IS_INT */

/* Define if using setrlimit to set resource limits during "make check" */
#define _GLIBCXX_RES_LIMITS 1

/* Define if size_t is unsigned int. */
/* #undef _GLIBCXX_SIZE_T_IS_UINT */

/* Define to the value of the EOF integer constant. */
#define _GLIBCXX_STDIO_EOF -1

/* Define to the value of the SEEK_CUR integer constant. */
#define _GLIBCXX_STDIO_SEEK_CUR 1

/* Define to the value of the SEEK_END integer constant. */
#define _GLIBCXX_STDIO_SEEK_END 2

/* Define to use symbol versioning in the shared library. */
#define _GLIBCXX_SYMVER 1

/* Define to use darwin versioning in the shared library. */
/* #undef _GLIBCXX_SYMVER_DARWIN */

/* Define to use GNU versioning in the shared library. */
#define _GLIBCXX_SYMVER_GNU 1

/* Define to use GNU namespace versioning in the shared library. */
/* #undef _GLIBCXX_SYMVER_GNU_NAMESPACE */

/* Define to use Sun versioning in the shared library. */
/* #undef _GLIBCXX_SYMVER_SUN */

/* Define if C11 functions in <uchar.h> should be imported into namespace std
   in <cuchar>. */
#define _GLIBCXX_USE_C11_UCHAR_CXX11 1

/* Define if C99 functions or macros from <wchar.h>, <math.h>, <complex.h>,
   <stdio.h>, and <stdlib.h> can be used or exposed. */
#define _GLIBCXX_USE_C99 1

/* Define if C99 functions in <complex.h> should be used in <tr1/complex>.
   Using compiler builtins for these functions requires corresponding C99
   library functions to be present. */
#define _GLIBCXX_USE_C99_COMPLEX_TR1 1

/* Define if C99 functions in <ctype.h> should be imported in <tr1/cctype> in
   namespace std::tr1. */
#define _GLIBCXX_USE_C99_CTYPE_TR1 1

/* Define if C99 functions in <fenv.h> should be imported in <tr1/cfenv> in
   namespace std::tr1. */
#define _GLIBCXX_USE_C99_FENV_TR1 1

/* Define if C99 functions in <inttypes.h> should be imported in
   <tr1/cinttypes> in namespace std::tr1. */
#define _GLIBCXX_USE_C99_INTTYPES_TR1 1

/* Define if wchar_t C99 functions in <inttypes.h> should be imported in
   <tr1/cinttypes> in namespace std::tr1. */
#define _GLIBCXX_USE_C99_INTTYPES_WCHAR_T_TR1 1

/* Define if C99 functions or macros in <math.h> should be imported in
   <tr1/cmath> in namespace std::tr1. */
#define _GLIBCXX_USE_C99_MATH_TR1 1

/* Define if C99 types in <stdint.h> should be imported in <tr1/cstdint> in
   namespace std::tr1. */
#define _GLIBCXX_USE_C99_STDINT_TR1 1

/* Defined if clock_gettime syscall has monotonic and realtime clock support.
   */
/* #undef _GLIBCXX_USE_CLOCK_GETTIME_SYSCALL */

/* Defined if clock_gettime has monotonic clock support. */
#define _GLIBCXX_USE_CLOCK_MONOTONIC 1

/* Defined if clock_gettime has realtime clock support. */
#define _GLIBCXX_USE_CLOCK_REALTIME 1

/* Define if ISO/IEC TR 24733 decimal floating point types are supported on
   this host. */
#define _GLIBCXX_USE_DECIMAL_FLOAT 1

/* Define if /dev/random and /dev/urandom are available for
   std::random_device. */
#define _GLIBCXX_USE_DEV_RANDOM 1

/* Define if fchmod is available in <sys/stat.h>. */
#define _GLIBCXX_USE_FCHMOD 1

/* Define if fchmodat is available in <sys/stat.h>. */
#define _GLIBCXX_USE_FCHMODAT 1

/* Defined if gettimeofday is available. */
#define _GLIBCXX_USE_GETTIMEOFDAY 1

/* Define if get_nprocs is available in <sys/sysinfo.h>. */
#define _GLIBCXX_USE_GET_NPROCS 1

/* Define if __int128 is supported on this host. */
#define _GLIBCXX_USE_INT128 1

/* Define if LFS support is available. */
#define _GLIBCXX_USE_LFS 1

/* Define if code specialized for long long should be used. */
#define _GLIBCXX_USE_LONG_LONG 1

/* Define if lstat is available in <sys/stat.h>. */
#define _GLIBCXX_USE_LSTAT 1

/* Defined if nanosleep is available. */
#define _GLIBCXX_USE_NANOSLEEP 1

/* Define if NLS translations are to be used. */
#define _GLIBCXX_USE_NLS 1

/* Define if pthreads_num_processors_np is available in <pthread.h>. */
/* #undef _GLIBCXX_USE_PTHREADS_NUM_PROCESSORS_NP */

/* Define if pthread_cond_clockwait is available in <pthread.h>. */
/* #undef _GLIBCXX_USE_PTHREAD_COND_CLOCKWAIT */

/* Define if pthread_mutex_clocklock is available in <pthread.h>. */
/* #undef _GLIBCXX_USE_PTHREAD_MUTEX_CLOCKLOCK */

/* Define if pthread_rwlock_clockrdlock and pthread_rwlock_clockwrlock are
   available in <pthread.h>. */
/* #undef _GLIBCXX_USE_PTHREAD_RWLOCK_CLOCKLOCK */

/* Define if POSIX read/write locks are available in <gthr.h>. */
#define _GLIBCXX_USE_PTHREAD_RWLOCK_T 1

/* Define if /dev/random and /dev/urandom are available for the random_device
   of TR1 (Chapter 5.1). */
#define _GLIBCXX_USE_RANDOM_TR1 1

/* Define if usable realpath is available in <stdlib.h>. */
#define _GLIBCXX_USE_REALPATH 1

/* Defined if sched_yield is available. */
#define _GLIBCXX_USE_SCHED_YIELD 1

/* Define if _SC_NPROCESSORS_ONLN is available in <unistd.h>. */
#define _GLIBCXX_USE_SC_NPROCESSORS_ONLN 1

/* Define if _SC_NPROC_ONLN is available in <unistd.h>. */
/* #undef _GLIBCXX_USE_SC_NPROC_ONLN */

/* Define if sendfile is available in <sys/sendfile.h>. */
#define _GLIBCXX_USE_SENDFILE 1

/* Define to restrict std::__basic_file<> to stdio APIs. */
/* #undef _GLIBCXX_USE_STDIO_PURE */

/* Define if struct stat has timespec members. */
#define _GLIBCXX_USE_ST_MTIM 1

/* Define if sysctl(), CTL_HW and HW_NCPU are available in <sys/sysctl.h>. */
/* #undef _GLIBCXX_USE_SYSCTL_HW_NCPU */

/* Define if obsolescent tmpnam is available in <stdio.h>. */
#define _GLIBCXX_USE_TMPNAM 1

/* Define if utime is available in <utime.h>. */
#define _GLIBCXX_USE_UTIME 1

/* Define if utimensat and UTIME_OMIT are available in <sys/stat.h> and
   AT_FDCWD in <fcntl.h>. */
#define _GLIBCXX_USE_UTIMENSAT 1

/* Define if code specialized for wchar_t should be used. */
#define _GLIBCXX_USE_WCHAR_T 1

/* Define to 1 if a verbose library is built, or 0 otherwise. */
#define _GLIBCXX_VERBOSE 1

/* Defined if as can handle rdrand. */
#define _GLIBCXX_X86_RDRAND 1

/* Defined if as can handle rdseed. */
#define _GLIBCXX_X86_RDSEED 1

/* Define to 1 if mutex_timedlock is available. */
#define _GTHREAD_USE_MUTEX_TIMEDLOCK 1

/* Define for large files, on AIX-style hosts. */
/* #undef _GLIBCXX_LARGE_FILES */

/* Define if all C++11 floating point overloads are available in <math.h>.  */

/* #undef __CORRECT_ISO_CPP11_MATH_H_PROTO_FP */


/* Define if all C++11 integral type overloads are available in <math.h>.  */

/* #undef __CORRECT_ISO_CPP11_MATH_H_PROTO_INT */


































































































































































































































































































































// Output streams -*- C++ -*-

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/ostream
 *  This is a Standard C++ Library header.
 */

//
// ISO C++ 14882: 27.6.2  Output streams
//


#define _GLIBCXX_OSTREAM 1

       

// Iostreams base classes -*- C++ -*-

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/ios
 *  This is a Standard C++ Library header.
 */

//
// ISO C++ 14882: 27.4  Iostreams base classes
//


#define _GLIBCXX_IOS 1

       

// <iosfwd> Forward declarations -*- C++ -*-

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/iosfwd
 *  This is a Standard C++ Library header.
 */

//
// ISO C++ 14882: 27.2  Forward declarations
//


#define _GLIBCXX_IOSFWD 1

       


// <string> Forward declarations -*- C++ -*-

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/stringfwd.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{string}
 */

//
// ISO C++ 14882: 21 Strings library
//


#define _STRINGFWD_H 1

       


// <memory> Forward declarations -*- C++ -*-

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/*
 * Copyright (c) 1996-1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/** @file bits/memoryfwd.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory}
 */


#define _MEMORYFWD_H 1

       



namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   * @defgroup allocators Allocators
   * @ingroup memory
   *
   * Classes encapsulating memory operations.
   *
   * @{
   */

  template<typename>
    class allocator;


  template<>
    class allocator<void>;



  /// Declare uses_allocator so it can be specialized in \<queue\> etc.
  template<typename, typename>
    struct uses_allocator;


  /// @} group memory

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std


namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @defgroup strings Strings
   *
   *  @{
  */

  template<class _CharT>
    struct char_traits;

  template<> struct char_traits<char>;


  template<> struct char_traits<wchar_t>;







  template<> struct char_traits<char16_t>;
  template<> struct char_traits<char32_t>;


_GLIBCXX_BEGIN_NAMESPACE_CXX11

  template<typename _CharT, typename _Traits = char_traits<_CharT>,
           typename _Alloc = allocator<_CharT> >
    class basic_string;

_GLIBCXX_END_NAMESPACE_CXX11

  /// A string of @c char
  typedef basic_string<char>    string;   


  /// A string of @c wchar_t
  typedef basic_string<wchar_t> wstring;   








  /// A string of @c char16_t
  typedef basic_string<char16_t> u16string; 

  /// A string of @c char32_t
  typedef basic_string<char32_t> u32string; 


  /** @}  */

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

// Position types -*- C++ -*-

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/postypes.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iosfwd}
 */

//
// ISO C++ 14882: 27.4.1 - Types
// ISO C++ 14882: 27.4.3 - Template class fpos
//


#define _GLIBCXX_POSTYPES_H 1

       

// -*- C++ -*- forwarding header.

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/cwchar
 *  This is a Standard C++ Library file.  You should @c \#include this file
 *  in your programs, rather than any of the @a *.h implementation files.
 *
 *  This is the C++ version of the Standard C Library header @c wchar.h,
 *  and its contents are (mostly) the same as that header, but are all
 *  contained in the namespace @c std (except for names which are defined
 *  as macros in C).
 */

//
// ISO C++ 14882: 21.4
//

       




/* Copyright (C) 1995-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *      ISO C99 Standard: 7.24
 *	Extended multibyte and wide character utilities	<wchar.h>
 */


#define _WCHAR_H 1

#define __GLIBC_INTERNAL_STARTING_HEADER_IMPLEMENTATION 
/* Handle feature test macros at the start of a header.
   Copyright (C) 2016-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* This header is internal to glibc and should not be included outside
   of glibc headers.  Headers including it must define
   __GLIBC_INTERNAL_STARTING_HEADER_IMPLEMENTATION first.  This header
   cannot have multiple include guards because ISO C feature test
   macros depend on the definition of the macro when an affected
   header is included, not when the first system header is
   included.  */





#undef __GLIBC_INTERNAL_STARTING_HEADER_IMPLEMENTATION



/* ISO/IEC TR 24731-2:2010 defines the __STDC_WANT_LIB_EXT2__
   macro.  */
#undef __GLIBC_USE_LIB_EXT2


#define __GLIBC_USE_LIB_EXT2 1




/* ISO/IEC TS 18661-1:2014 defines the __STDC_WANT_IEC_60559_BFP_EXT__
   macro.  */
#undef __GLIBC_USE_IEC_60559_BFP_EXT

#define __GLIBC_USE_IEC_60559_BFP_EXT 1




/* ISO/IEC TS 18661-4:2015 defines the
   __STDC_WANT_IEC_60559_FUNCS_EXT__ macro.  */
#undef __GLIBC_USE_IEC_60559_FUNCS_EXT

#define __GLIBC_USE_IEC_60559_FUNCS_EXT 1




/* ISO/IEC TS 18661-3:2015 defines the
   __STDC_WANT_IEC_60559_TYPES_EXT__ macro.  */
#undef __GLIBC_USE_IEC_60559_TYPES_EXT

#define __GLIBC_USE_IEC_60559_TYPES_EXT 1

/* Gather machine dependent type support.  */
/* Macros to control TS 18661-3 glibc features on x86.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_FLOATN_H 



/* Defined to 1 if the current compiler invocation provides a
   floating-point type with the IEEE 754 binary128 format, and this
   glibc includes corresponding *f128 interfaces for it.  The required
   libgcc support was added some time after the basic compiler
   support, for x86_64 and x86.  */



#define __HAVE_FLOAT128 1




/* Defined to 1 if __HAVE_FLOAT128 is 1 and the type is ABI-distinct
   from the default float, double and long double types in this glibc.  */

#define __HAVE_DISTINCT_FLOAT128 1




/* Defined to 1 if the current compiler invocation provides a
   floating-point type with the right format for _Float64x, and this
   glibc includes corresponding *f64x interfaces for it.  */
#define __HAVE_FLOAT64X 1

/* Defined to 1 if __HAVE_FLOAT64X is 1 and _Float64x has the format
   of long double.  Otherwise, if __HAVE_FLOAT64X is 1, _Float64x has
   the format of _Float128, which must be different from that of long
   double.  */
#define __HAVE_FLOAT64X_LONG_DOUBLE 1



/* Defined to concatenate the literal suffix to be used with _Float128
   types, if __HAVE_FLOAT128 is 1. */


/* The literal suffix f128 exists only since GCC 7.0.  */
#define __f128(x) x ##q





/* Defined to a complex binary128 type if __HAVE_FLOAT128 is 1.  */


/* Add a typedef for older GCC compilers which don't natively support
   _Complex _Float128.  */
typedef _Complex float __cfloat128 __attribute__ ((__mode__ (__TC__)));
#define __CFLOAT128 __cfloat128





/* The remaining of this file provides support for older compilers.  */


/* The type _Float128 exists only since GCC 7.0.  */

typedef __float128 _Float128;


/* __builtin_huge_valf128 doesn't exist before GCC 7.0.  */




/* Older GCC has only a subset of built-in functions for _Float128 on
   x86, and __builtin_infq is not usable in static initializers.
   Converting a narrower sNaN to _Float128 produces a quiet NaN, so
   attempts to use _Float128 sNaNs will not work properly with older
   compilers.  */








/* In math/math.h, __MATH_TG will expand signbit to __builtin_signbit*,
   e.g.: __builtin_signbitf128, before GCC 6.  However, there has never
   been a __builtin_signbitf128 in GCC and the type-generic builtin is
   only available since GCC 6.  */








/* Macros to control TS 18661-3 glibc features where the same
   definitions are appropriate for all platforms.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_FLOATN_COMMON_H 


/* Properties of long double type.  ldbl-96 version.
   Copyright (C) 2016-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License  published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* long double is distinct from double, so there is nothing to
   define here.  */

/* This header should be included at the bottom of each bits/floatn.h.
   It defines the following macros for each _FloatN and _FloatNx type,
   where the same definitions, or definitions based only on the macros
   in bits/floatn.h, are appropriate for all glibc configurations.  */

/* Defined to 1 if the current compiler invocation provides a
   floating-point type with the right format for this type, and this
   glibc includes corresponding *fN or *fNx interfaces for it.  */
#define __HAVE_FLOAT16 0
#define __HAVE_FLOAT32 1
#define __HAVE_FLOAT64 1
#define __HAVE_FLOAT32X 1
#define __HAVE_FLOAT128X 0

/* Defined to 1 if the corresponding __HAVE_<type> macro is 1 and the
   type is the first with its format in the sequence of (the default
   choices for) float, double, long double, _Float16, _Float32,
   _Float64, _Float128, _Float32x, _Float64x, _Float128x for this
   glibc; that is, if functions present once per floating-point format
   rather than once per type are present for this type.

   All configurations supported by glibc have _Float32 the same format
   as float, _Float64 and _Float32x the same format as double, the
   _Float64x the same format as either long double or _Float128.  No
   configurations support _Float128x or, as of GCC 7, have compiler
   support for a type meeting the requirements for _Float128x.  */
#define __HAVE_DISTINCT_FLOAT16 __HAVE_FLOAT16
#define __HAVE_DISTINCT_FLOAT32 0
#define __HAVE_DISTINCT_FLOAT64 0
#define __HAVE_DISTINCT_FLOAT32X 0
#define __HAVE_DISTINCT_FLOAT64X 0
#define __HAVE_DISTINCT_FLOAT128X __HAVE_FLOAT128X

/* Defined to 1 if the corresponding _FloatN type is not binary compatible
   with the corresponding ISO C type in the current compilation unit as
   opposed to __HAVE_DISTINCT_FLOATN, which indicates the default types built
   in glibc.  */
#define __HAVE_FLOAT128_UNLIKE_LDBL (__HAVE_DISTINCT_FLOAT128 && __LDBL_MANT_DIG__ != 113)


/* Defined to 1 if any _FloatN or _FloatNx types that are not
   ABI-distinct are however distinct types at the C language level (so
   for the purposes of __builtin_types_compatible_p and _Generic).  */



#define __HAVE_FLOATN_NOT_TYPEDEF 0




/* Defined to concatenate the literal suffix to be used with _FloatN
   or _FloatNx types, if __HAVE_<type> is 1.  The corresponding
   literal suffixes exist since GCC 7, for C only.  */



#define __f32(x) x ##f










#define __f64(x) x








#define __f32x(x) x








#define __f64x(x) x ##l
















/* Defined to a complex type if __HAVE_<type> is 1.  */



#define __CFLOAT32 _Complex float










#define __CFLOAT64 _Complex double








#define __CFLOAT32X _Complex double








#define __CFLOAT64X _Complex long double
















/* The remaining of this file provides support for older compilers.  */




typedef float _Float32;













/* If double, long double and _Float64 all have the same set of
   values, TS 18661-3 requires the usual arithmetic conversions on
   long double and _Float64 to produce _Float64.  For this to be the
   case when building with a compiler without a distinct _Float64
   type, _Float64 must be a typedef for long double, not for
   double.  */



typedef double _Float64;
















typedef double _Float32x;
















typedef long double _Float64x;

















#define __need_size_t 
#define __need_wchar_t 
#define __need_NULL 
/* Copyright (C) 1989-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */






/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */


/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */










/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */




/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */













/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */

/* Signed type of difference of two pointers.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/* Unsigned type of `sizeof' something.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


















#define __size_t__ 
#define __SIZE_T__ 
#define _SIZE_T 
#define _SYS_SIZE_T_H 
#define _T_SIZE_ 
#define _T_SIZE 
#define __SIZE_T 
#define _SIZE_T_ 
#define _BSD_SIZE_T_ 
#define _SIZE_T_DEFINED_ 
#define _SIZE_T_DEFINED 
#define _BSD_SIZE_T_DEFINED_ 
#define _SIZE_T_DECLARED 
#define ___int_size_t_h 
#define _GCC_SIZE_T 
#define _SIZET_ 






#define __size_t 





typedef __SIZE_TYPE__ size_t;





















#undef __need_size_t



/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


















#define __wchar_t__ 
#define __WCHAR_T__ 
#define _WCHAR_T 
#define _T_WCHAR_ 
#define _T_WCHAR 
#define __WCHAR_T 
#define _WCHAR_T_ 
#define _BSD_WCHAR_T_ 
#define _WCHAR_T_DEFINED_ 
#define _WCHAR_T_DEFINED 
#define _WCHAR_T_H 
#define ___int_wchar_t_h 
#define __INT_WCHAR_T_H 
#define _GCC_WCHAR_T 
#define _WCHAR_T_DECLARED 

/* On BSD/386 1.1, at least, machine/ansi.h defines _BSD_WCHAR_T_
   instead of _WCHAR_T_, and _BSD_RUNE_T_ (which, unlike the other
   symbols in the _FOO_T_ family, stays defined even after its
   corresponding type is defined).  If we define wchar_t, then we
   must undef _WCHAR_T_; for BSD/386 1.1 (and perhaps others), if
   we undef _WCHAR_T_, then we must also define rune_t, since 
   headers like runetype.h assume that if machine/ansi.h is included,
   and _BSD_WCHAR_T_ is not defined, then rune_t is available.
   machine/ansi.h says, "Note that _WCHAR_T_ and _RUNE_T_ must be of
   the same type." */

#undef _BSD_WCHAR_T_

/* FreeBSD 5 can't be handled well using "traditional" logic above
   since it no longer defines _BSD_RUNE_T_ yet still desires to export
   rune_t in some cases... */
























#undef __need_wchar_t






/* A null pointer constant.  */


#undef NULL

#define NULL __null








#undef __need_NULL



#define __need___va_list 
/* Copyright (C) 1989-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.15  Variable arguments  <stdarg.h>
 */







#undef __need___va_list

/* Define __gnuc_va_list.  */


#define __GNUC_VA_LIST 
typedef __builtin_va_list __gnuc_va_list;


/* Define the standard macros for the user,
   if this invocation was from the user program.  */



/* wchar_t type related definitions.
   Copyright (C) 2000-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_WCHAR_H 1

/* The fallback definitions, for when __WCHAR_MAX__ or __WCHAR_MIN__
   are not defined, give the right value and type as long as both int
   and wchar_t are 32-bit types.  Adding L'\0' to a constant value
   ensures that the type is correct; it is necessary to use (L'\0' +
   0) rather than just L'\0' so that the type in C++ is the promoted
   version of wchar_t rather than the distinct wchar_t type itself.
   Because wchar_t in preprocessor #if expressions is treated as
   intmax_t or uintmax_t, the expression (L'\0' - 1) would have the
   wrong value for WCHAR_MAX in such expressions and so cannot be used
   to define __WCHAR_MAX in the unsigned case.  */


#define __WCHAR_MAX __WCHAR_MAX__







#define __WCHAR_MIN __WCHAR_MIN__







#define __wint_t_defined 1

/* Some versions of stddef.h provide wint_t, even though neither the
   C nor C++ standards, nor POSIX, specifies this.  We assume that
   stddef.h will define the macro _WINT_T if and only if it provides
   wint_t, and conversely, that it will avoid providing wint_t if
   _WINT_T is already defined.  */

#define _WINT_T 1

/* Integral type unchanged by default argument promotions that can
   hold any value corresponding to members of the extended character
   set, as well as at least one value that does not correspond to any
   member of the extended character set.  */




typedef __WINT_TYPE__ wint_t;



#define __mbstate_t_defined 1


#define ____mbstate_t_defined 1

/* Integral type unchanged by default argument promotions that can
   hold any value corresponding to members of the extended character
   set, as well as at least one value that does not correspond to any
   member of the extended character set.  */




/* Conversion state information.  */
typedef struct
{
  int __count;
  union
  {
    __WINT_TYPE__ __wch;
    char __wchb[4];
  } __value;		/* Value so far.  */
} __mbstate_t;


typedef __mbstate_t mbstate_t;


#define ____FILE_defined 1

struct _IO_FILE;
typedef struct _IO_FILE __FILE;




#define __FILE_defined 1

struct _IO_FILE;

/* The opaque type of streams.  This is the definition used elsewhere.  */
typedef struct _IO_FILE FILE;



/* Definition of locale_t.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_TYPES_LOCALE_T_H 1

/* Definition of struct __locale_struct and __locale_t.
   Copyright (C) 1997-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_TYPES___LOCALE_T_H 1

/* POSIX.1-2008: the locale_t type, representing a locale context
   (implementation-namespace version).  This type should be treated
   as opaque by applications; some details are exposed for the sake of
   efficiency in e.g. ctype functions.  */

struct __locale_struct
{
  /* Note: LC_ALL is not a valid index into this array.  */
  struct __locale_data *__locales[13]; /* 13 = __LC_LAST. */

  /* To increase the speed of this solution we add some special members.  */
  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;

  /* Note: LC_ALL is not a valid index into this array.  */
  const char *__names[13];
};

typedef struct __locale_struct *__locale_t;


typedef __locale_t locale_t;



/* Tell the caller that we provide correct C++ prototypes.  */

#define __CORRECT_ISO_CPP_WCHAR_H_PROTO 



/* These constants might also be defined in <inttypes.h>.  */
#define WCHAR_MIN __WCHAR_MIN
#define WCHAR_MAX __WCHAR_MAX



#define WEOF (0xffffffffu)


/* All versions of XPG prior to the publication of ISO C99 required
   the bulk of <wctype.h>'s declarations to appear in this header
   (because <wctype.h> did not exist prior to C99).  In POSIX.1-2001
   those declarations were marked as XSI extensions; in -2008 they
   were additionally marked as obsolescent.  _GNU_SOURCE mode
   anticipates the removal of these declarations in the next revision
   of POSIX.  */





__BEGIN_DECLS

/* This incomplete type is defined in <time.h> but needed here because
   of `wcsftime'.  */
struct tm;


/* Copy SRC to DEST.  */
extern wchar_t *wcscpy (wchar_t *__restrict __dest,
			const wchar_t *__restrict __src)
     __THROW __nonnull ((1, 2));

/* Copy no more than N wide-characters of SRC to DEST.  */
extern wchar_t *wcsncpy (wchar_t *__restrict __dest,
			 const wchar_t *__restrict __src, size_t __n)
     __THROW __nonnull ((1, 2));

/* Append SRC onto DEST.  */
extern wchar_t *wcscat (wchar_t *__restrict __dest,
			const wchar_t *__restrict __src)
     __THROW __nonnull ((1, 2));
/* Append no more than N wide-characters of SRC onto DEST.  */
extern wchar_t *wcsncat (wchar_t *__restrict __dest,
			 const wchar_t *__restrict __src, size_t __n)
     __THROW __nonnull ((1, 2));

/* Compare S1 and S2.  */
extern int wcscmp (const wchar_t *__s1, const wchar_t *__s2)
     __THROW __attribute_pure__ __nonnull ((1, 2));
/* Compare N wide-characters of S1 and S2.  */
extern int wcsncmp (const wchar_t *__s1, const wchar_t *__s2, size_t __n)
     __THROW __attribute_pure__ __nonnull ((1, 2));


/* Compare S1 and S2, ignoring case.  */
extern int wcscasecmp (const wchar_t *__s1, const wchar_t *__s2) __THROW;

/* Compare no more than N chars of S1 and S2, ignoring case.  */
extern int wcsncasecmp (const wchar_t *__s1, const wchar_t *__s2,
			size_t __n) __THROW;

/* Similar to the two functions above but take the information from
   the provided locale and not the global locale.  */
extern int wcscasecmp_l (const wchar_t *__s1, const wchar_t *__s2,
			 locale_t __loc) __THROW;

extern int wcsncasecmp_l (const wchar_t *__s1, const wchar_t *__s2,
			  size_t __n, locale_t __loc) __THROW;


/* Compare S1 and S2, both interpreted as appropriate to the
   LC_COLLATE category of the current locale.  */
extern int wcscoll (const wchar_t *__s1, const wchar_t *__s2) __THROW;
/* Transform S2 into array pointed to by S1 such that if wcscmp is
   applied to two transformed strings the result is the as applying
   `wcscoll' to the original strings.  */
extern size_t wcsxfrm (wchar_t *__restrict __s1,
		       const wchar_t *__restrict __s2, size_t __n) __THROW;


/* Similar to the two functions above but take the information from
   the provided locale and not the global locale.  */

/* Compare S1 and S2, both interpreted as appropriate to the
   LC_COLLATE category of the given locale.  */
extern int wcscoll_l (const wchar_t *__s1, const wchar_t *__s2,
		      locale_t __loc) __THROW;

/* Transform S2 into array pointed to by S1 such that if wcscmp is
   applied to two transformed strings the result is the as applying
   `wcscoll' to the original strings.  */
extern size_t wcsxfrm_l (wchar_t *__s1, const wchar_t *__s2,
			 size_t __n, locale_t __loc) __THROW;

/* Duplicate S, returning an identical malloc'd string.  */
extern wchar_t *wcsdup (const wchar_t *__s) __THROW __attribute_malloc__;


/* Find the first occurrence of WC in WCS.  */

extern "C++" wchar_t *wcschr (wchar_t *__wcs, wchar_t __wc)
     __THROW __asm ("wcschr") __attribute_pure__;
extern "C++" const wchar_t *wcschr (const wchar_t *__wcs, wchar_t __wc)
     __THROW __asm ("wcschr") __attribute_pure__;




/* Find the last occurrence of WC in WCS.  */

extern "C++" wchar_t *wcsrchr (wchar_t *__wcs, wchar_t __wc)
     __THROW __asm ("wcsrchr") __attribute_pure__;
extern "C++" const wchar_t *wcsrchr (const wchar_t *__wcs, wchar_t __wc)
     __THROW __asm ("wcsrchr") __attribute_pure__;






/* This function is similar to `wcschr'.  But it returns a pointer to
   the closing NUL wide character in case C is not found in S.  */
extern wchar_t *wcschrnul (const wchar_t *__s, wchar_t __wc)
     __THROW __attribute_pure__;


/* Return the length of the initial segmet of WCS which
   consists entirely of wide characters not in REJECT.  */
extern size_t wcscspn (const wchar_t *__wcs, const wchar_t *__reject)
     __THROW __attribute_pure__;
/* Return the length of the initial segmet of WCS which
   consists entirely of wide characters in  ACCEPT.  */
extern size_t wcsspn (const wchar_t *__wcs, const wchar_t *__accept)
     __THROW __attribute_pure__;
/* Find the first occurrence in WCS of any character in ACCEPT.  */

extern "C++" wchar_t *wcspbrk (wchar_t *__wcs, const wchar_t *__accept)
     __THROW __asm ("wcspbrk") __attribute_pure__;
extern "C++" const wchar_t *wcspbrk (const wchar_t *__wcs,
				     const wchar_t *__accept)
     __THROW __asm ("wcspbrk") __attribute_pure__;




/* Find the first occurrence of NEEDLE in HAYSTACK.  */

extern "C++" wchar_t *wcsstr (wchar_t *__haystack, const wchar_t *__needle)
     __THROW __asm ("wcsstr") __attribute_pure__;
extern "C++" const wchar_t *wcsstr (const wchar_t *__haystack,
				    const wchar_t *__needle)
     __THROW __asm ("wcsstr") __attribute_pure__;





/* Divide WCS into tokens separated by characters in DELIM.  */
extern wchar_t *wcstok (wchar_t *__restrict __s,
			const wchar_t *__restrict __delim,
			wchar_t **__restrict __ptr) __THROW;

/* Return the number of wide characters in S.  */
extern size_t wcslen (const wchar_t *__s) __THROW __attribute_pure__;


/* Another name for `wcsstr' from XPG4.  */

extern "C++" wchar_t *wcswcs (wchar_t *__haystack, const wchar_t *__needle)
     __THROW __asm ("wcswcs") __attribute_pure__;
extern "C++" const wchar_t *wcswcs (const wchar_t *__haystack,
				    const wchar_t *__needle)
     __THROW __asm ("wcswcs") __attribute_pure__;







/* Return the number of wide characters in S, but at most MAXLEN.  */
extern size_t wcsnlen (const wchar_t *__s, size_t __maxlen)
     __THROW __attribute_pure__;



/* Search N wide characters of S for C.  */

extern "C++" wchar_t *wmemchr (wchar_t *__s, wchar_t __c, size_t __n)
     __THROW __asm ("wmemchr") __attribute_pure__;
extern "C++" const wchar_t *wmemchr (const wchar_t *__s, wchar_t __c,
				     size_t __n)
     __THROW __asm ("wmemchr") __attribute_pure__;





/* Compare N wide characters of S1 and S2.  */
extern int wmemcmp (const wchar_t *__s1, const wchar_t *__s2, size_t __n)
     __THROW __attribute_pure__;

/* Copy N wide characters of SRC to DEST.  */
extern wchar_t *wmemcpy (wchar_t *__restrict __s1,
			 const wchar_t *__restrict __s2, size_t __n) __THROW;

/* Copy N wide characters of SRC to DEST, guaranteeing
   correct behavior for overlapping strings.  */
extern wchar_t *wmemmove (wchar_t *__s1, const wchar_t *__s2, size_t __n)
     __THROW;

/* Set N wide characters of S to C.  */
extern wchar_t *wmemset (wchar_t *__s, wchar_t __c, size_t __n) __THROW;


/* Copy N wide characters of SRC to DEST and return pointer to following
   wide character.  */
extern wchar_t *wmempcpy (wchar_t *__restrict __s1,
			  const wchar_t *__restrict __s2, size_t __n)
     __THROW;



/* Determine whether C constitutes a valid (one-byte) multibyte
   character.  */
extern wint_t btowc (int __c) __THROW;

/* Determine whether C corresponds to a member of the extended
   character set whose multibyte representation is a single byte.  */
extern int wctob (wint_t __c) __THROW;

/* Determine whether PS points to an object representing the initial
   state.  */
extern int mbsinit (const mbstate_t *__ps) __THROW __attribute_pure__;

/* Write wide character representation of multibyte character pointed
   to by S to PWC.  */
extern size_t mbrtowc (wchar_t *__restrict __pwc,
		       const char *__restrict __s, size_t __n,
		       mbstate_t *__restrict __p) __THROW;

/* Write multibyte representation of wide character WC to S.  */
extern size_t wcrtomb (char *__restrict __s, wchar_t __wc,
		       mbstate_t *__restrict __ps) __THROW;

/* Return number of bytes in multibyte character pointed to by S.  */
extern size_t __mbrlen (const char *__restrict __s, size_t __n,
			mbstate_t *__restrict __ps) __THROW;
extern size_t mbrlen (const char *__restrict __s, size_t __n,
		      mbstate_t *__restrict __ps) __THROW;


/* Write wide character representation of multibyte character string
   SRC to DST.  */
extern size_t mbsrtowcs (wchar_t *__restrict __dst,
			 const char **__restrict __src, size_t __len,
			 mbstate_t *__restrict __ps) __THROW;

/* Write multibyte character representation of wide character string
   SRC to DST.  */
extern size_t wcsrtombs (char *__restrict __dst,
			 const wchar_t **__restrict __src, size_t __len,
			 mbstate_t *__restrict __ps) __THROW;



/* Write wide character representation of at most NMC bytes of the
   multibyte character string SRC to DST.  */
extern size_t mbsnrtowcs (wchar_t *__restrict __dst,
			  const char **__restrict __src, size_t __nmc,
			  size_t __len, mbstate_t *__restrict __ps) __THROW;

/* Write multibyte character representation of at most NWC characters
   from the wide character string SRC to DST.  */
extern size_t wcsnrtombs (char *__restrict __dst,
			  const wchar_t **__restrict __src,
			  size_t __nwc, size_t __len,
			  mbstate_t *__restrict __ps) __THROW;



/* The following functions are extensions found in X/Open CAE.  */

/* Determine number of column positions required for C.  */
extern int wcwidth (wchar_t __c) __THROW;

/* Determine number of column positions required for first N wide
   characters (or fewer if S ends before this) in S.  */
extern int wcswidth (const wchar_t *__s, size_t __n) __THROW;



/* Convert initial portion of the wide string NPTR to `double'
   representation.  */
extern double wcstod (const wchar_t *__restrict __nptr,
		      wchar_t **__restrict __endptr) __THROW;


/* Likewise for `float' and `long double' sizes of floating-point numbers.  */
extern float wcstof (const wchar_t *__restrict __nptr,
		     wchar_t **__restrict __endptr) __THROW;
extern long double wcstold (const wchar_t *__restrict __nptr,
			    wchar_t **__restrict __endptr) __THROW;


/* Likewise for `_FloatN' and `_FloatNx' when support is enabled.  */







extern _Float32 wcstof32 (const wchar_t *__restrict __nptr,
			  wchar_t **__restrict __endptr) __THROW;



extern _Float64 wcstof64 (const wchar_t *__restrict __nptr,
			  wchar_t **__restrict __endptr) __THROW;



extern _Float128 wcstof128 (const wchar_t *__restrict __nptr,
			    wchar_t **__restrict __endptr) __THROW;



extern _Float32x wcstof32x (const wchar_t *__restrict __nptr,
			    wchar_t **__restrict __endptr) __THROW;



extern _Float64x wcstof64x (const wchar_t *__restrict __nptr,
			    wchar_t **__restrict __endptr) __THROW;








/* Convert initial portion of wide string NPTR to `long int'
   representation.  */
extern long int wcstol (const wchar_t *__restrict __nptr,
			wchar_t **__restrict __endptr, int __base) __THROW;

/* Convert initial portion of wide string NPTR to `unsigned long int'
   representation.  */
extern unsigned long int wcstoul (const wchar_t *__restrict __nptr,
				  wchar_t **__restrict __endptr, int __base)
     __THROW;


/* Convert initial portion of wide string NPTR to `long long int'
   representation.  */
__extension__
extern long long int wcstoll (const wchar_t *__restrict __nptr,
			      wchar_t **__restrict __endptr, int __base)
     __THROW;

/* Convert initial portion of wide string NPTR to `unsigned long long int'
   representation.  */
__extension__
extern unsigned long long int wcstoull (const wchar_t *__restrict __nptr,
					wchar_t **__restrict __endptr,
					int __base) __THROW;



/* Convert initial portion of wide string NPTR to `long long int'
   representation.  */
__extension__
extern long long int wcstoq (const wchar_t *__restrict __nptr,
			     wchar_t **__restrict __endptr, int __base)
     __THROW;

/* Convert initial portion of wide string NPTR to `unsigned long long int'
   representation.  */
__extension__
extern unsigned long long int wcstouq (const wchar_t *__restrict __nptr,
				       wchar_t **__restrict __endptr,
				       int __base) __THROW;



/* Parallel versions of the functions above which take the locale to
   use as an additional parameter.  These are GNU extensions inspired
   by the POSIX.1-2008 extended locale API.  */
extern long int wcstol_l (const wchar_t *__restrict __nptr,
			  wchar_t **__restrict __endptr, int __base,
			  locale_t __loc) __THROW;

extern unsigned long int wcstoul_l (const wchar_t *__restrict __nptr,
				    wchar_t **__restrict __endptr,
				    int __base, locale_t __loc) __THROW;

__extension__
extern long long int wcstoll_l (const wchar_t *__restrict __nptr,
				wchar_t **__restrict __endptr,
				int __base, locale_t __loc) __THROW;

__extension__
extern unsigned long long int wcstoull_l (const wchar_t *__restrict __nptr,
					  wchar_t **__restrict __endptr,
					  int __base, locale_t __loc)
     __THROW;

extern double wcstod_l (const wchar_t *__restrict __nptr,
			wchar_t **__restrict __endptr, locale_t __loc)
     __THROW;

extern float wcstof_l (const wchar_t *__restrict __nptr,
		       wchar_t **__restrict __endptr, locale_t __loc)
     __THROW;

extern long double wcstold_l (const wchar_t *__restrict __nptr,
			      wchar_t **__restrict __endptr,
			      locale_t __loc) __THROW;








extern _Float32 wcstof32_l (const wchar_t *__restrict __nptr,
			    wchar_t **__restrict __endptr,
			    locale_t __loc) __THROW;



extern _Float64 wcstof64_l (const wchar_t *__restrict __nptr,
			    wchar_t **__restrict __endptr,
			    locale_t __loc) __THROW;



extern _Float128 wcstof128_l (const wchar_t *__restrict __nptr,
			      wchar_t **__restrict __endptr,
			      locale_t __loc) __THROW;



extern _Float32x wcstof32x_l (const wchar_t *__restrict __nptr,
			      wchar_t **__restrict __endptr,
			      locale_t __loc) __THROW;



extern _Float64x wcstof64x_l (const wchar_t *__restrict __nptr,
			      wchar_t **__restrict __endptr,
			      locale_t __loc) __THROW;











/* Copy SRC to DEST, returning the address of the terminating L'\0' in
   DEST.  */
extern wchar_t *wcpcpy (wchar_t *__restrict __dest,
			const wchar_t *__restrict __src) __THROW;

/* Copy no more than N characters of SRC to DEST, returning the address of
   the last character written into DEST.  */
extern wchar_t *wcpncpy (wchar_t *__restrict __dest,
			 const wchar_t *__restrict __src, size_t __n)
     __THROW;



/* Wide character I/O functions.  */


/* Like OPEN_MEMSTREAM, but the stream is wide oriented and produces
   a wide character string.  */
extern __FILE *open_wmemstream (wchar_t **__bufloc, size_t *__sizeloc) __THROW;




/* Select orientation for stream.  */
extern int fwide (__FILE *__fp, int __mode) __THROW;


/* Write formatted output to STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fwprintf (__FILE *__restrict __stream,
		     const wchar_t *__restrict __format, ...)
     /* __attribute__ ((__format__ (__wprintf__, 2, 3))) */;
/* Write formatted output to stdout.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int wprintf (const wchar_t *__restrict __format, ...)
     /* __attribute__ ((__format__ (__wprintf__, 1, 2))) */;
/* Write formatted output of at most N characters to S.  */
extern int swprintf (wchar_t *__restrict __s, size_t __n,
		     const wchar_t *__restrict __format, ...)
     __THROW /* __attribute__ ((__format__ (__wprintf__, 3, 4))) */;

/* Write formatted output to S from argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int vfwprintf (__FILE *__restrict __s,
		      const wchar_t *__restrict __format,
		      __gnuc_va_list __arg)
     /* __attribute__ ((__format__ (__wprintf__, 2, 0))) */;
/* Write formatted output to stdout from argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int vwprintf (const wchar_t *__restrict __format,
		     __gnuc_va_list __arg)
     /* __attribute__ ((__format__ (__wprintf__, 1, 0))) */;
/* Write formatted output of at most N character to S from argument
   list ARG.  */
extern int vswprintf (wchar_t *__restrict __s, size_t __n,
		      const wchar_t *__restrict __format,
		      __gnuc_va_list __arg)
     __THROW /* __attribute__ ((__format__ (__wprintf__, 3, 0))) */;


/* Read formatted input from STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fwscanf (__FILE *__restrict __stream,
		    const wchar_t *__restrict __format, ...)
     /* __attribute__ ((__format__ (__wscanf__, 2, 3))) */;
/* Read formatted input from stdin.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int wscanf (const wchar_t *__restrict __format, ...)
     /* __attribute__ ((__format__ (__wscanf__, 1, 2))) */;
/* Read formatted input from S.  */
extern int swscanf (const wchar_t *__restrict __s,
		    const wchar_t *__restrict __format, ...)
     __THROW /* __attribute__ ((__format__ (__wscanf__, 2, 3))) */;





/* Read formatted input from S into argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int vfwscanf (__FILE *__restrict __s,
		     const wchar_t *__restrict __format,
		     __gnuc_va_list __arg)
     /* __attribute__ ((__format__ (__wscanf__, 2, 0))) */;
/* Read formatted input from stdin into argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int vwscanf (const wchar_t *__restrict __format,
		    __gnuc_va_list __arg)
     /* __attribute__ ((__format__ (__wscanf__, 1, 0))) */;
/* Read formatted input from S into argument list ARG.  */
extern int vswscanf (const wchar_t *__restrict __s,
		     const wchar_t *__restrict __format,
		     __gnuc_va_list __arg)
     __THROW /* __attribute__ ((__format__ (__wscanf__, 2, 0))) */;





/* Read a character from STREAM.

   These functions are possible cancellation points and therefore not
   marked with __THROW.  */
extern wint_t fgetwc (__FILE *__stream);
extern wint_t getwc (__FILE *__stream);

/* Read a character from stdin.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern wint_t getwchar (void);


/* Write a character to STREAM.

   These functions are possible cancellation points and therefore not
   marked with __THROW.  */
extern wint_t fputwc (wchar_t __wc, __FILE *__stream);
extern wint_t putwc (wchar_t __wc, __FILE *__stream);

/* Write a character to stdout.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern wint_t putwchar (wchar_t __wc);


/* Get a newline-terminated wide character string of finite length
   from STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern wchar_t *fgetws (wchar_t *__restrict __ws, int __n,
			__FILE *__restrict __stream);

/* Write a string to STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fputws (const wchar_t *__restrict __ws,
		   __FILE *__restrict __stream);


/* Push a character back onto the input buffer of STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern wint_t ungetwc (wint_t __wc, __FILE *__stream);



/* These are defined to be equivalent to the `char' functions defined
   in POSIX.1:1996.

   These functions are not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation they are cancellation points and
   therefore not marked with __THROW.  */
extern wint_t getwc_unlocked (__FILE *__stream);
extern wint_t getwchar_unlocked (void);

/* This is the wide character version of a GNU extension.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */
extern wint_t fgetwc_unlocked (__FILE *__stream);

/* Faster version when locking is not necessary.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */
extern wint_t fputwc_unlocked (wchar_t __wc, __FILE *__stream);

/* These are defined to be equivalent to the `char' functions defined
   in POSIX.1:1996.

   These functions are not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation they are cancellation points and
   therefore not marked with __THROW.  */
extern wint_t putwc_unlocked (wchar_t __wc, __FILE *__stream);
extern wint_t putwchar_unlocked (wchar_t __wc);


/* This function does the same as `fgetws' but does not lock the stream.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */
extern wchar_t *fgetws_unlocked (wchar_t *__restrict __ws, int __n,
				 __FILE *__restrict __stream);

/* This function does the same as `fputws' but does not lock the stream.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */
extern int fputws_unlocked (const wchar_t *__restrict __ws,
			    __FILE *__restrict __stream);



/* Format TP into S according to FORMAT.
   Write no more than MAXSIZE wide characters and return the number
   of wide characters written, or 0 if it would exceed MAXSIZE.  */
extern size_t wcsftime (wchar_t *__restrict __s, size_t __maxsize,
			const wchar_t *__restrict __format,
			const struct tm *__restrict __tp) __THROW;


/* Similar to `wcsftime' but takes the information from
   the provided locale and not the global locale.  */
extern size_t wcsftime_l (wchar_t *__restrict __s, size_t __maxsize,
			  const wchar_t *__restrict __format,
			  const struct tm *__restrict __tp,
			  locale_t __loc) __THROW;


/* Define some macros helping to catch buffer overflows.  */








__END_DECLS




#define _GLIBCXX_CWCHAR 1

// Need to do a bit of trickery here with mbstate_t as char_traits
// assumes it is in wchar.h, regardless of wchar_t specializations.

namespace std
{
  using ::mbstate_t;
} // namespace std

// Get rid of those macros defined in <wchar.h> in lieu of real functions.
#undef btowc
#undef fgetwc
#undef fgetws
#undef fputwc
#undef fputws
#undef fwide
#undef fwprintf
#undef fwscanf
#undef getwc
#undef getwchar
#undef mbrlen
#undef mbrtowc
#undef mbsinit
#undef mbsrtowcs
#undef putwc
#undef putwchar
#undef swprintf
#undef swscanf
#undef ungetwc
#undef vfwprintf

#undef vfwscanf

#undef vswprintf

#undef vswscanf

#undef vwprintf

#undef vwscanf

#undef wcrtomb
#undef wcscat
#undef wcschr
#undef wcscmp
#undef wcscoll
#undef wcscpy
#undef wcscspn
#undef wcsftime
#undef wcslen
#undef wcsncat
#undef wcsncmp
#undef wcsncpy
#undef wcspbrk
#undef wcsrchr
#undef wcsrtombs
#undef wcsspn
#undef wcsstr
#undef wcstod

#undef wcstof

#undef wcstok
#undef wcstol
#undef wcstoul
#undef wcsxfrm
#undef wctob
#undef wmemchr
#undef wmemcmp
#undef wmemcpy
#undef wmemmove
#undef wmemset
#undef wprintf
#undef wscanf



extern "C++"
{
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  using ::wint_t;

  using ::btowc;
  using ::fgetwc;
  using ::fgetws;
  using ::fputwc;
  using ::fputws;
  using ::fwide;
  using ::fwprintf;
  using ::fwscanf;
  using ::getwc;
  using ::getwchar;
  using ::mbrlen;
  using ::mbrtowc;
  using ::mbsinit;
  using ::mbsrtowcs;
  using ::putwc;
  using ::putwchar;

  using ::swprintf;

  using ::swscanf;
  using ::ungetwc;
  using ::vfwprintf;

  using ::vfwscanf;


  using ::vswprintf;


  using ::vswscanf;

  using ::vwprintf;

  using ::vwscanf;

  using ::wcrtomb;
  using ::wcscat;
  using ::wcscmp;
  using ::wcscoll;
  using ::wcscpy;
  using ::wcscspn;
  using ::wcsftime;
  using ::wcslen;
  using ::wcsncat;
  using ::wcsncmp;
  using ::wcsncpy;
  using ::wcsrtombs;
  using ::wcsspn;
  using ::wcstod;

  using ::wcstof;

  using ::wcstok;
  using ::wcstol;
  using ::wcstoul;
  using ::wcsxfrm;
  using ::wctob;
  using ::wmemcmp;
  using ::wmemcpy;
  using ::wmemmove;
  using ::wmemset;
  using ::wprintf;
  using ::wscanf;
  using ::wcschr;
  using ::wcspbrk;
  using ::wcsrchr;
  using ::wcsstr;
  using ::wmemchr;


_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
} // extern "C++"



#undef wcstold
#undef wcstoll
#undef wcstoull

namespace __gnu_cxx
{





  using ::wcstold;








  using ::wcstoll;
  using ::wcstoull;

} // namespace __gnu_cxx

namespace std
{
  using ::__gnu_cxx::wcstold;
  using ::__gnu_cxx::wcstoll;
  using ::__gnu_cxx::wcstoull;
} // namespace









namespace std
{

  using std::wcstof;


  using std::vfwscanf;


  using std::vswscanf;


  using std::vwscanf;



  using std::wcstold;
  using std::wcstoll;
  using std::wcstoull;

} // namespace






// XXX If <stdint.h> is really needed, make sure to define the macros
// before including it, in order not to break <tr1/cstdint> (and <cstdint>
// in C++11).  Reconsider all this as soon as possible...

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // The types streamoff, streampos and wstreampos and the class
  // template fpos<> are described in clauses 21.1.2, 21.1.3, 27.1.2,
  // 27.2, 27.4.1, 27.4.3 and D.6. Despite all this verbiage, the
  // behaviour of these types is mostly implementation defined or
  // unspecified. The behaviour in this implementation is as noted
  // below.

  /**
   *  @brief  Type used by fpos, char_traits<char>, and char_traits<wchar_t>.
   *
   *  In clauses 21.1.3.1 and 27.4.1 streamoff is described as an
   *  implementation defined type.
   *  Note: In versions of GCC up to and including GCC 3.3, streamoff
   *  was typedef long.
  */  

  typedef long          streamoff;








  /// Integral type for I/O operation counts and buffer sizes.
  typedef ptrdiff_t	streamsize; // Signed integral type

  /**
   *  @brief  Class representing stream positions.
   *
   *  The standard places no requirements upon the template parameter StateT.
   *  In this implementation StateT must be DefaultConstructible,
   *  CopyConstructible and Assignable.  The standard only requires that fpos
   *  should contain a member of type StateT. In this implementation it also
   *  contains an offset stored as a signed integer.
   *
   *  @param  StateT  Type passed to and returned from state().
   */
  template<typename _StateT>
    class fpos
    {
    private:
      streamoff	                _M_off;
      _StateT			_M_state;

    public:
      // The standard doesn't require that fpos objects can be default
      // constructed. This implementation provides a default
      // constructor that initializes the offset to 0 and default
      // constructs the state.
      fpos()
      : _M_off(0), _M_state() { }

      // The standard requires that fpos objects can be constructed
      // from streamoff objects using the constructor syntax, and
      // fails to give any meaningful semantics. In this
      // implementation implicit conversion is also allowed, and this
      // constructor stores the streamoff as the offset and default
      // constructs the state.
      /// Construct position from offset.
      fpos(streamoff __off)
      : _M_off(__off), _M_state() { }


      fpos(const fpos&) = default;
      fpos& operator=(const fpos&) = default;
      ~fpos() = default;


      /// Convert to streamoff.
      operator streamoff() const { return _M_off; }

      /// Remember the value of @a st.
      void
      state(_StateT __st)
      { _M_state = __st; }

      /// Return the last set value of @a st.
      _StateT
      state() const
      { return _M_state; }

      // The standard requires that this operator must be defined, but
      // gives no semantics. In this implementation it just adds its
      // argument to the stored offset and returns *this.
      /// Add offset to this position.
      fpos&
      operator+=(streamoff __off)
      {
	_M_off += __off;
	return *this;
      }

      // The standard requires that this operator must be defined, but
      // gives no semantics. In this implementation it just subtracts
      // its argument from the stored offset and returns *this.
      /// Subtract offset from this position.
      fpos&
      operator-=(streamoff __off)
      {
	_M_off -= __off;
	return *this;
      }

      // The standard requires that this operator must be defined, but
      // defines its semantics only in terms of operator-. In this
      // implementation it constructs a copy of *this, adds the
      // argument to that copy using operator+= and then returns the
      // copy.
      /// Add position and offset.
      fpos
      operator+(streamoff __off) const
      {
	fpos __pos(*this);
	__pos += __off;
	return __pos;
      }

      // The standard requires that this operator must be defined, but
      // defines its semantics only in terms of operator+. In this
      // implementation it constructs a copy of *this, subtracts the
      // argument from that copy using operator-= and then returns the
      // copy.
      /// Subtract offset from position.
      fpos
      operator-(streamoff __off) const
      {
	fpos __pos(*this);
	__pos -= __off;
	return __pos;
      }

      // The standard requires that this operator must be defined, but
      // defines its semantics only in terms of operator+. In this
      // implementation it returns the difference between the offset
      // stored in *this and in the argument.
      /// Subtract position to return offset.
      streamoff
      operator-(const fpos& __other) const
      { return _M_off - __other._M_off; }
    };

  // The standard only requires that operator== must be an
  // equivalence relation. In this implementation two fpos<StateT>
  // objects belong to the same equivalence class if the contained
  // offsets compare equal.
  /// Test if equivalent to another position.
  template<typename _StateT>
    inline bool
    operator==(const fpos<_StateT>& __lhs, const fpos<_StateT>& __rhs)
    { return streamoff(__lhs) == streamoff(__rhs); }

  template<typename _StateT>
    inline bool
    operator!=(const fpos<_StateT>& __lhs, const fpos<_StateT>& __rhs)
    { return streamoff(__lhs) != streamoff(__rhs); }

  // Clauses 21.1.3.1 and 21.1.3.2 describe streampos and wstreampos
  // as implementation defined types, but clause 27.2 requires that
  // they must both be typedefs for fpos<mbstate_t>
  /// File position for char streams.
  typedef fpos<mbstate_t> streampos;
  /// File position for wchar_t streams.
  typedef fpos<mbstate_t> wstreampos;







  /// File position for char16_t streams.
  typedef fpos<mbstate_t> u16streampos;
  /// File position for char32_t streams.
  typedef fpos<mbstate_t> u32streampos;


_GLIBCXX_END_NAMESPACE_VERSION
} // namespace


namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @defgroup io I/O
   *
   *  Nearly all of the I/O classes are parameterized on the type of
   *  characters they read and write.  (The major exception is ios_base at
   *  the top of the hierarchy.)  This is a change from pre-Standard
   *  streams, which were not templates.
   *
   *  For ease of use and compatibility, all of the basic_* I/O-related
   *  classes are given typedef names for both of the builtin character
   *  widths (wide and narrow).  The typedefs are the same as the
   *  pre-Standard names, for example:
   *
   *  @code
   *     typedef basic_ifstream<char>  ifstream;
   *  @endcode
   *
   *  Because properly forward-declaring these classes can be difficult, you
   *  should not do it yourself.  Instead, include the &lt;iosfwd&gt;
   *  header, which contains only declarations of all the I/O classes as
   *  well as the typedefs.  Trying to forward-declare the typedefs
   *  themselves (e.g., <code>class ostream;</code>) is not valid ISO C++.
   *
   *  For more specific declarations, see
   *  https://gcc.gnu.org/onlinedocs/libstdc++/manual/io.html#std.io.objects
   *
   *  @{
  */
  class ios_base;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
    class basic_ios;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
    class basic_streambuf;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
    class basic_istream;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
    class basic_ostream;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
    class basic_iostream;


_GLIBCXX_BEGIN_NAMESPACE_CXX11

  template<typename _CharT, typename _Traits = char_traits<_CharT>,
	    typename _Alloc = allocator<_CharT> >
    class basic_stringbuf;

  template<typename _CharT, typename _Traits = char_traits<_CharT>,
	   typename _Alloc = allocator<_CharT> >
    class basic_istringstream;

  template<typename _CharT, typename _Traits = char_traits<_CharT>,
	   typename _Alloc = allocator<_CharT> >
    class basic_ostringstream;

  template<typename _CharT, typename _Traits = char_traits<_CharT>,
	   typename _Alloc = allocator<_CharT> >
    class basic_stringstream;

_GLIBCXX_END_NAMESPACE_CXX11

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
    class basic_filebuf;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
    class basic_ifstream;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
    class basic_ofstream;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
    class basic_fstream;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
    class istreambuf_iterator;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
    class ostreambuf_iterator;


  /// Base class for @c char streams.
  typedef basic_ios<char> 		ios;

  /// Base class for @c char buffers.
  typedef basic_streambuf<char> 	streambuf;

  /// Base class for @c char input streams.
  typedef basic_istream<char> 		istream;

  /// Base class for @c char output streams.
  typedef basic_ostream<char> 		ostream;

  /// Base class for @c char mixed input and output streams.
  typedef basic_iostream<char> 		iostream;

  /// Class for @c char memory buffers.
  typedef basic_stringbuf<char> 	stringbuf;

  /// Class for @c char input memory streams.
  typedef basic_istringstream<char> 	istringstream;

  /// Class for @c char output memory streams.
  typedef basic_ostringstream<char> 	ostringstream;

  /// Class for @c char mixed input and output memory streams.
  typedef basic_stringstream<char> 	stringstream;

  /// Class for @c char file buffers.
  typedef basic_filebuf<char> 		filebuf;

  /// Class for @c char input file streams.
  typedef basic_ifstream<char> 		ifstream;

  /// Class for @c char output file streams.
  typedef basic_ofstream<char> 		ofstream;

  /// Class for @c char mixed input and output file streams.
  typedef basic_fstream<char> 		fstream;


  /// Base class for @c wchar_t streams.
  typedef basic_ios<wchar_t> 		wios;

  /// Base class for @c wchar_t buffers.
  typedef basic_streambuf<wchar_t> 	wstreambuf;

  /// Base class for @c wchar_t input streams.
  typedef basic_istream<wchar_t> 	wistream;

  /// Base class for @c wchar_t output streams.
  typedef basic_ostream<wchar_t> 	wostream;

  /// Base class for @c wchar_t mixed input and output streams.
  typedef basic_iostream<wchar_t> 	wiostream;

  /// Class for @c wchar_t memory buffers.
  typedef basic_stringbuf<wchar_t> 	wstringbuf;

  /// Class for @c wchar_t input memory streams.
  typedef basic_istringstream<wchar_t> 	wistringstream;

  /// Class for @c wchar_t output memory streams.
  typedef basic_ostringstream<wchar_t> 	wostringstream;

  /// Class for @c wchar_t mixed input and output memory streams.
  typedef basic_stringstream<wchar_t> 	wstringstream;

  /// Class for @c wchar_t file buffers.
  typedef basic_filebuf<wchar_t> 	wfilebuf;

  /// Class for @c wchar_t input file streams.
  typedef basic_ifstream<wchar_t> 	wifstream;

  /// Class for @c wchar_t output file streams.
  typedef basic_ofstream<wchar_t> 	wofstream;

  /// Class for @c wchar_t mixed input and output file streams.
  typedef basic_fstream<wchar_t> 	wfstream;

  /** @}  */

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// Exception Handling support header for -*- C++ -*-

// Copyright (C) 1995-2021 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file exception
 *  This is a Standard C++ Library header.
 */


#define __EXCEPTION__ 

       

#pragma GCC visibility push(default)


// Exception Handling support header for -*- C++ -*-

// Copyright (C) 2016-2021 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/exception.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly.
 */


#define __EXCEPTION_H 1

       

#pragma GCC visibility push(default)



extern "C++" {

namespace std
{
  /**
   * @defgroup exceptions Exceptions
   * @ingroup diagnostics
   *
   * Classes and functions for reporting errors via exception classes.
   * @{
   */

  /**
   *  @brief Base class for all library exceptions.
   *
   *  This is the base class for all exceptions thrown by the standard
   *  library, and by certain language expressions.  You are free to derive
   *  your own %exception classes, or use a different hierarchy, or to
   *  throw non-class data (e.g., fundamental types).
   */
  class exception
  {
  public:
    exception() _GLIBCXX_NOTHROW { }
    virtual ~exception() _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_NOTHROW;

    exception(const exception&) = default;
    exception& operator=(const exception&) = default;
    exception(exception&&) = default;
    exception& operator=(exception&&) = default;


    /** Returns a C-style character string describing the general cause
     *  of the current error.  */
    virtual const char*
    what() const _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_NOTHROW;
  };

} // namespace std

}

#pragma GCC visibility pop


extern "C++" {

namespace std
{
  /** @addtogroup exceptions
   *  @{
   */

  /** If an %exception is thrown which is not listed in a function's
   *  %exception specification, one of these may be thrown.  */
  class bad_exception : public exception
  {
  public:
    bad_exception() _GLIBCXX_USE_NOEXCEPT { }

    // This declaration is not useless:
    // http://gcc.gnu.org/onlinedocs/gcc-3.0.2/gcc_6.html#SEC118
    virtual ~bad_exception() _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_USE_NOEXCEPT;

    // See comment in eh_exception.cc.
    virtual const char*
    what() const _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_USE_NOEXCEPT;
  };

  /// If you write a replacement %terminate handler, it must be of this type.
  typedef void (*terminate_handler) ();

  /// If you write a replacement %unexpected handler, it must be of this type.
  typedef void (*unexpected_handler) ();

  /// Takes a new handler function as an argument, returns the old function.
  terminate_handler set_terminate(terminate_handler) _GLIBCXX_USE_NOEXCEPT;


  /// Return the current terminate handler.
  terminate_handler get_terminate() noexcept;


  /** The runtime will call this function if %exception handling must be
   *  abandoned for any reason.  It can also be called by the user.  */
  void terminate() _GLIBCXX_USE_NOEXCEPT __attribute__ ((__noreturn__));

  /// Takes a new handler function as an argument, returns the old function.
  unexpected_handler set_unexpected(unexpected_handler) _GLIBCXX_USE_NOEXCEPT;


  /// Return the current unexpected handler.
  unexpected_handler get_unexpected() noexcept;


  /** The runtime will call this function if an %exception is thrown which
   *  violates the function's %exception specification.  */
  void unexpected() __attribute__ ((__noreturn__));

  /** [18.6.4]/1:  'Returns true after completing evaluation of a
   *  throw-expression until either completing initialization of the
   *  exception-declaration in the matching handler or entering @c unexpected()
   *  due to the throw; or after entering @c terminate() for any reason
   *  other than an explicit call to @c terminate().  [Note: This includes
   *  stack unwinding [15.2].  end note]'
   *
   *  2: 'When @c uncaught_exception() is true, throwing an
   *  %exception can result in a call of @c terminate()
   *  (15.5.1).'
   */
  _GLIBCXX17_DEPRECATED
  bool uncaught_exception() _GLIBCXX_USE_NOEXCEPT __attribute__ ((__pure__));


#define __cpp_lib_uncaught_exceptions 201411L
  /// The number of uncaught exceptions.
  int uncaught_exceptions() _GLIBCXX_USE_NOEXCEPT __attribute__ ((__pure__));


  // @} group exceptions
} // namespace std

namespace __gnu_cxx
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @brief A replacement for the standard terminate_handler which
   *  prints more information about the terminating exception (if any)
   *  on stderr.
   *
   *  @ingroup exceptions
   *
   *  Call
   *   @code
   *     std::set_terminate(__gnu_cxx::__verbose_terminate_handler)
   *   @endcode
   *  to use.  For more info, see
   *  http://gcc.gnu.org/onlinedocs/libstdc++/manual/bk01pt02ch06s02.html
   *
   *  In 3.4 and later, this is on by default.
   */
  void __verbose_terminate_handler();

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

} // extern "C++"

#pragma GCC visibility pop


// Exception Handling support header (exception_ptr class) for -*- C++ -*-

// Copyright (C) 2008-2021 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
// 
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/exception_ptr.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{exception}
 */


#define _EXCEPTION_PTR_H 

#pragma GCC visibility push(default)


// -fno-exceptions Support -*- C++ -*-

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/exception_defines.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{exception}
 */


#define _EXCEPTION_DEFINES_H 1







// Else proceed normally.
#define __try try
#define __catch(X) catch(X)
#define __throw_exception_again throw


// ABI Support -*- C++ -*-

// Copyright (C) 2016-2021 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/cxxabi_init_exception.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly.
 */


#define _CXXABI_INIT_EXCEPTION_H 1

       

#pragma GCC visibility push(default)

/* Copyright (C) 1989-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */






/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */



#define _STDDEF_H 
#define _STDDEF_H_ 
/* snaroff@next.com says the NeXT needs this.  */
#define _ANSI_STDDEF_H 



/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */










/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */




/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */













/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */

/* Signed type of difference of two pointers.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */










#define _PTRDIFF_T 
#define _T_PTRDIFF_ 
#define _T_PTRDIFF 
#define __PTRDIFF_T 
#define _PTRDIFF_T_ 
#define _BSD_PTRDIFF_T_ 
#define ___int_ptrdiff_t_h 
#define _GCC_PTRDIFF_T 
#define _PTRDIFF_T_DECLARED 



typedef __PTRDIFF_TYPE__ ptrdiff_t;










/* If this symbol has done its job, get rid of it.  */
#undef __need_ptrdiff_t



/* Unsigned type of `sizeof' something.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

#undef __need_size_t



/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

#undef __need_wchar_t






/* A null pointer constant.  */


#undef NULL

#define NULL __null








#undef __need_NULL



/* Offset of member MEMBER in a struct of type TYPE. */
#define offsetof(TYPE,MEMBER) __builtin_offsetof (TYPE, MEMBER)




#define _GCC_MAX_ALIGN_T 
/* Type whose alignment is supported in every context and is at least
   as great as that of any standard type not using alignment
   specifiers.  */
typedef struct {
  long long __max_align_ll __attribute__((__aligned__(__alignof__(long long))));
  long double __max_align_ld __attribute__((__aligned__(__alignof__(long double))));
  /* _Float128 is defined as a basic type, so max_align_t must be
     sufficiently aligned for it.  This code must work in C++, so we
     use __float128 here; that is only available on some
     architectures, but only on i386 is extra alignment needed for
     __float128.  */



} max_align_t;





#define _GXX_NULLPTR_T 
  typedef decltype(nullptr) nullptr_t;








#define _GLIBCXX_CDTOR_CALLABI 
#define _GLIBCXX_HAVE_CDTOR_CALLABI 0






namespace std
{
  class type_info;
}

namespace __cxxabiv1
{
  struct __cxa_refcounted_exception;

  extern "C"
    {
      // Allocate memory for the primary exception plus the thrown object.
      void*
      __cxa_allocate_exception(size_t) _GLIBCXX_NOTHROW;

      void
      __cxa_free_exception(void*) _GLIBCXX_NOTHROW;

      // Initialize exception (this is a GNU extension)
      __cxa_refcounted_exception*
      __cxa_init_primary_exception(void *object, std::type_info *tinfo,
                void (_GLIBCXX_CDTOR_CALLABI *dest) (void *)) _GLIBCXX_NOTHROW;

    }
} // namespace __cxxabiv1



#pragma GCC visibility pop

// RTTI support for -*- C++ -*-
// Copyright (C) 1994-2021 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file typeinfo
 *  This is a Standard C++ Library header.
 */


#define _TYPEINFO 

       



// Declarations for hash functions. -*- C++ -*-

// Copyright (C) 2010-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/hash_bytes.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{functional}
 */


#define _HASH_BYTES_H 1

       



namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Hash function implementation for the nontrivial specialization.
  // All of them are based on a primitive that hashes a pointer to a
  // byte array. The actual hash algorithm is not guaranteed to stay
  // the same from release to release -- it may be updated or tuned to
  // improve hash quality or speed.
  size_t
  _Hash_bytes(const void* __ptr, size_t __len, size_t __seed);

  // A similar hash primitive, using the FNV hash algorithm. This
  // algorithm is guaranteed to stay the same from release to release.
  // (although it might not produce the same values on different
  // machines.)
  size_t
  _Fnv_hash_bytes(const void* __ptr, size_t __len, size_t __seed);

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace



#pragma GCC visibility push(default)

extern "C++" {

namespace __cxxabiv1
{
  class __class_type_info;
} // namespace __cxxabiv1

// Determine whether typeinfo names for the same type are merged (in which
// case comparison can just compare pointers) or not (in which case strings
// must be compared), and whether comparison is to be implemented inline or
// not.  We used to do inline pointer comparison by default if weak symbols
// are available, but even with weak symbols sometimes names are not merged
// when objects are loaded with RTLD_LOCAL, so now we always use strcmp by
// default.  For ABI compatibility, we do the strcmp inline if weak symbols
// are available, and out-of-line if not.  Out-of-line pointer comparison
// is used where the object files are to be portable to multiple systems,
// some of which may not be able to use pointer comparison, but the
// particular system for which libstdc++ is being built can use pointer
// comparison; in particular for most ARM EABI systems, where the ABI
// specifies out-of-line comparison.  The compiler's target configuration
// can override the defaults by defining __GXX_TYPEINFO_EQUALITY_INLINE to
// 1 or 0 to indicate whether or not comparison is inline, and
// __GXX_MERGED_TYPEINFO_NAMES to 1 or 0 to indicate whether or not pointer
// comparison can be used.


// By default, typeinfo names are not merged.
#define __GXX_MERGED_TYPEINFO_NAMES 0


// By default follow the old inline rules to avoid ABI changes.

  


    #define __GXX_TYPEINFO_EQUALITY_INLINE 1
  


namespace std
{
  /**
   *  @brief  Part of RTTI.
   *
   *  The @c type_info class describes type information generated by
   *  an implementation.
  */
  class type_info
  {
  public:
    /** Destructor first. Being the first non-inline virtual function, this
     *  controls in which translation unit the vtable is emitted. The
     *  compiler makes use of that information to know where to emit
     *  the runtime-mandated type_info structures in the new-abi.  */
    virtual ~type_info();

    /** Returns an @e implementation-defined byte string; this is not
     *  portable between compilers!  */
    const char* name() const _GLIBCXX_NOEXCEPT
    { return __name[0] == '*' ? __name + 1 : __name; }








  
    /** Returns true if @c *this precedes @c __arg in the implementation's
     *  collation order.  */
    // Even with the new abi, on systems that support dlopen
    // we can run into cases where type_info names aren't merged,
    // so we still need to do string comparison.
    bool before(const type_info& __arg) const _GLIBCXX_NOEXCEPT
    { return (__name[0] == '*' && __arg.__name[0] == '*')
	? __name < __arg.__name
	: __builtin_strcmp (__name, __arg.__name) < 0; }

    bool operator==(const type_info& __arg) const _GLIBCXX_NOEXCEPT
    {
      return ((__name == __arg.__name)
	      || (__name[0] != '*' &&
		  __builtin_strcmp (__name, __arg.__name) == 0));
    }



    bool operator!=(const type_info& __arg) const _GLIBCXX_NOEXCEPT
    { return !operator==(__arg); }



    size_t hash_code() const noexcept
    {

      return _Hash_bytes(name(), __builtin_strlen(name()),
			 static_cast<size_t>(0xc70f6907UL));



    }


    // Return true if this is a pointer type of some kind
    virtual bool __is_pointer_p() const;

    // Return true if this is a function type
    virtual bool __is_function_p() const;

    // Try and catch a thrown type. Store an adjusted pointer to the
    // caught type in THR_OBJ. If THR_TYPE is not a pointer type, then
    // THR_OBJ points to the thrown object. If THR_TYPE is a pointer
    // type, then THR_OBJ is the pointer itself. OUTER indicates the
    // number of outer pointers, and whether they were const
    // qualified.
    virtual bool __do_catch(const type_info *__thr_type, void **__thr_obj,
			    unsigned __outer) const;

    // Internally used during catch matching
    virtual bool __do_upcast(const __cxxabiv1::__class_type_info *__target,
			     void **__obj_ptr) const;

  protected:
    const char *__name;

    explicit type_info(const char *__n): __name(__n) { }

  private:
    /// Assigning type_info is not supported.
    type_info& operator=(const type_info&);
    type_info(const type_info&);
  };

  /**
   *  @brief  Thrown during incorrect typecasting.
   *  @ingroup exceptions
   *
   *  If you attempt an invalid @c dynamic_cast expression, an instance of
   *  this class (or something derived from this class) is thrown.  */
  class bad_cast : public exception
  {
  public:
    bad_cast() _GLIBCXX_USE_NOEXCEPT { }

    // This declaration is not useless:
    // http://gcc.gnu.org/onlinedocs/gcc-3.0.2/gcc_6.html#SEC118
    virtual ~bad_cast() _GLIBCXX_USE_NOEXCEPT;

    // See comment in eh_exception.cc.
    virtual const char* what() const _GLIBCXX_USE_NOEXCEPT;
  };

  /**
   *  @brief Thrown when a NULL pointer in a @c typeid expression is used.
   *  @ingroup exceptions
   */
  class bad_typeid : public exception
  {
  public:
    bad_typeid () _GLIBCXX_USE_NOEXCEPT { }

    // This declaration is not useless:
    // http://gcc.gnu.org/onlinedocs/gcc-3.0.2/gcc_6.html#SEC118
    virtual ~bad_typeid() _GLIBCXX_USE_NOEXCEPT;

    // See comment in eh_exception.cc.
    virtual const char* what() const _GLIBCXX_USE_NOEXCEPT;
  };
} // namespace std

} // extern "C++"

#pragma GCC visibility pop

// The -*- C++ -*- dynamic memory management header.

// Copyright (C) 1994-2021 Free Software Foundation, Inc.

// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
// 
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file new
 *  This is a Standard C++ Library header.
 *
 *  The header @c new defines several functions to manage dynamic memory and
 *  handling memory allocation errors; see
 *  https://gcc.gnu.org/onlinedocs/libstdc++/manual/dynamic_memory.html
 *  for more.
 */


#define _NEW 

       




#pragma GCC visibility push(default)

extern "C++" {

namespace std 
{
  /**
   *  @brief  Exception possibly thrown by @c new.
   *  @ingroup exceptions
   *
   *  @c bad_alloc (or classes derived from it) is used to report allocation
   *  errors from the throwing forms of @c new.  */
  class bad_alloc : public exception
  {
  public:
    bad_alloc() throw() { }


    bad_alloc(const bad_alloc&) = default;
    bad_alloc& operator=(const bad_alloc&) = default;


    // This declaration is not useless:
    // http://gcc.gnu.org/onlinedocs/gcc-3.0.2/gcc_6.html#SEC118
    virtual ~bad_alloc() throw();

    // See comment in eh_exception.cc.
    virtual const char* what() const throw();
  };


  class bad_array_new_length : public bad_alloc
  {
  public:
    bad_array_new_length() throw() { }

    // This declaration is not useless:
    // http://gcc.gnu.org/onlinedocs/gcc-3.0.2/gcc_6.html#SEC118
    virtual ~bad_array_new_length() throw();

    // See comment in eh_exception.cc.
    virtual const char* what() const throw();
  };



  enum class align_val_t: size_t {};


  struct nothrow_t
  {

    explicit nothrow_t() = default;

  };

  extern const nothrow_t nothrow;

  /** If you write your own error handler to be called by @c new, it must
   *  be of this type.  */
  typedef void (*new_handler)();

  /// Takes a replacement handler as the argument, returns the
  /// previous handler.
  new_handler set_new_handler(new_handler) throw();


  /// Return the current new handler.
  new_handler get_new_handler() noexcept;

} // namespace std

//@{
/** These are replaceable signatures:
 *  - normal single new and delete (no arguments, throw @c bad_alloc on error)
 *  - normal array new and delete (same)
 *  - @c nothrow single new and delete (take a @c nothrow argument, return
 *    @c NULL on error)
 *  - @c nothrow array new and delete (same)
 *
 *  Placement new and delete signatures (take a memory address argument,
 *  does nothing) may not be replaced by a user's program.
*/
_GLIBCXX_NODISCARD void* operator new(std::size_t) _GLIBCXX_THROW (std::bad_alloc)
  __attribute__((__externally_visible__));
_GLIBCXX_NODISCARD void* operator new[](std::size_t) _GLIBCXX_THROW (std::bad_alloc)
  __attribute__((__externally_visible__));
void operator delete(void*) _GLIBCXX_USE_NOEXCEPT
  __attribute__((__externally_visible__));
void operator delete[](void*) _GLIBCXX_USE_NOEXCEPT
  __attribute__((__externally_visible__));

void operator delete(void*, std::size_t) _GLIBCXX_USE_NOEXCEPT
  __attribute__((__externally_visible__));
void operator delete[](void*, std::size_t) _GLIBCXX_USE_NOEXCEPT
  __attribute__((__externally_visible__));

_GLIBCXX_NODISCARD void* operator new(std::size_t, const std::nothrow_t&) _GLIBCXX_USE_NOEXCEPT
  __attribute__((__externally_visible__, __alloc_size__ (1), __malloc__));
_GLIBCXX_NODISCARD void* operator new[](std::size_t, const std::nothrow_t&) _GLIBCXX_USE_NOEXCEPT
  __attribute__((__externally_visible__, __alloc_size__ (1), __malloc__));
void operator delete(void*, const std::nothrow_t&) _GLIBCXX_USE_NOEXCEPT
  __attribute__((__externally_visible__));
void operator delete[](void*, const std::nothrow_t&) _GLIBCXX_USE_NOEXCEPT
  __attribute__((__externally_visible__));

_GLIBCXX_NODISCARD void* operator new(std::size_t, std::align_val_t)
  __attribute__((__externally_visible__, __alloc_size__ (1), __malloc__));
_GLIBCXX_NODISCARD void* operator new(std::size_t, std::align_val_t, const std::nothrow_t&)
  _GLIBCXX_USE_NOEXCEPT __attribute__((__externally_visible__, __alloc_size__ (1), __malloc__));
void operator delete(void*, std::align_val_t)
  _GLIBCXX_USE_NOEXCEPT __attribute__((__externally_visible__));
void operator delete(void*, std::align_val_t, const std::nothrow_t&)
  _GLIBCXX_USE_NOEXCEPT __attribute__((__externally_visible__));
_GLIBCXX_NODISCARD void* operator new[](std::size_t, std::align_val_t)
  __attribute__((__externally_visible__, __alloc_size__ (1), __malloc__));
_GLIBCXX_NODISCARD void* operator new[](std::size_t, std::align_val_t, const std::nothrow_t&)
  _GLIBCXX_USE_NOEXCEPT __attribute__((__externally_visible__, __alloc_size__ (1), __malloc__));
void operator delete[](void*, std::align_val_t)
  _GLIBCXX_USE_NOEXCEPT __attribute__((__externally_visible__));
void operator delete[](void*, std::align_val_t, const std::nothrow_t&)
  _GLIBCXX_USE_NOEXCEPT __attribute__((__externally_visible__));

void operator delete(void*, std::size_t, std::align_val_t)
  _GLIBCXX_USE_NOEXCEPT __attribute__((__externally_visible__));
void operator delete[](void*, std::size_t, std::align_val_t)
  _GLIBCXX_USE_NOEXCEPT __attribute__((__externally_visible__));



// Default placement versions of operator new.
_GLIBCXX_NODISCARD inline void* operator new(std::size_t, void* __p) _GLIBCXX_USE_NOEXCEPT
{ return __p; }
_GLIBCXX_NODISCARD inline void* operator new[](std::size_t, void* __p) _GLIBCXX_USE_NOEXCEPT
{ return __p; }

// Default placement versions of operator delete.
inline void operator delete  (void*, void*) _GLIBCXX_USE_NOEXCEPT { }
inline void operator delete[](void*, void*) _GLIBCXX_USE_NOEXCEPT { }
//@}
} // extern "C++"



namespace std
{
#define __cpp_lib_launder 201606
  /// Pointer optimization barrier [ptr.launder]
  template<typename _Tp>
    [[nodiscard]] constexpr _Tp*
    launder(_Tp* __p) noexcept
    { return __builtin_launder(__p); }

  // The program is ill-formed if T is a function type or
  // (possibly cv-qualified) void.

  template<typename _Ret, typename... _Args _GLIBCXX_NOEXCEPT_PARM>
    void launder(_Ret (*)(_Args...) _GLIBCXX_NOEXCEPT_QUAL) = delete;
  template<typename _Ret, typename... _Args _GLIBCXX_NOEXCEPT_PARM>
    void launder(_Ret (*)(_Args......) _GLIBCXX_NOEXCEPT_QUAL) = delete;

  void launder(void*) = delete;
  void launder(const void*) = delete;
  void launder(volatile void*) = delete;
  void launder(const volatile void*) = delete;
}




#pragma GCC visibility pop





#define _GLIBCXX_EH_PTR_USED 


extern "C++" {

namespace std 
{
  class type_info;

  /**
   * @addtogroup exceptions
   * @{
   */

  namespace __exception_ptr
  {
    class exception_ptr;
  }

  using __exception_ptr::exception_ptr;

  /** Obtain an exception_ptr to the currently handled exception. If there
   *  is none, or the currently handled exception is foreign, return the null
   *  value.
   */
  exception_ptr current_exception() _GLIBCXX_USE_NOEXCEPT;

  template<typename _Ex>
  exception_ptr make_exception_ptr(_Ex) _GLIBCXX_USE_NOEXCEPT;

  /// Throw the object pointed to by the exception_ptr.
  void rethrow_exception(exception_ptr) __attribute__ ((__noreturn__));

  namespace __exception_ptr
  {
    using std::rethrow_exception;

    /**
     *  @brief An opaque pointer to an arbitrary exception.
     *  @ingroup exceptions
     */
    class exception_ptr
    {
      void* _M_exception_object;

      explicit exception_ptr(void* __e) _GLIBCXX_USE_NOEXCEPT;

      void _M_addref() _GLIBCXX_USE_NOEXCEPT;
      void _M_release() _GLIBCXX_USE_NOEXCEPT;

      void *_M_get() const _GLIBCXX_NOEXCEPT __attribute__ ((__pure__));

      friend exception_ptr std::current_exception() _GLIBCXX_USE_NOEXCEPT;
      friend void std::rethrow_exception(exception_ptr);
      template<typename _Ex>
      friend exception_ptr std::make_exception_ptr(_Ex) _GLIBCXX_USE_NOEXCEPT;

    public:
      exception_ptr() _GLIBCXX_USE_NOEXCEPT;

      exception_ptr(const exception_ptr&) _GLIBCXX_USE_NOEXCEPT;


      exception_ptr(nullptr_t) noexcept
      : _M_exception_object(nullptr)
      { }

      exception_ptr(exception_ptr&& __o) noexcept
      : _M_exception_object(__o._M_exception_object)
      { __o._M_exception_object = nullptr; }









      exception_ptr& 
      operator=(const exception_ptr&) _GLIBCXX_USE_NOEXCEPT;


      exception_ptr& 
      operator=(exception_ptr&& __o) noexcept
      {
        exception_ptr(static_cast<exception_ptr&&>(__o)).swap(*this);
        return *this;
      }


      ~exception_ptr() _GLIBCXX_USE_NOEXCEPT;

      void 
      swap(exception_ptr&) _GLIBCXX_USE_NOEXCEPT;



      explicit operator bool() const noexcept
      { return _M_exception_object; }







      friend _GLIBCXX_EH_PTR_USED bool
      operator==(const exception_ptr& __x, const exception_ptr& __y)
      _GLIBCXX_USE_NOEXCEPT
      { return __x._M_exception_object == __y._M_exception_object; }

      friend _GLIBCXX_EH_PTR_USED bool
      operator!=(const exception_ptr& __x, const exception_ptr& __y)
      _GLIBCXX_USE_NOEXCEPT
      { return __x._M_exception_object != __y._M_exception_object; }


      const class std::type_info*
      __cxa_exception_type() const _GLIBCXX_USE_NOEXCEPT
	__attribute__ ((__pure__));
    };

    _GLIBCXX_EH_PTR_USED
    inline
    exception_ptr::exception_ptr() _GLIBCXX_NOEXCEPT
    : _M_exception_object(0)
    { }

    _GLIBCXX_EH_PTR_USED
    inline
    exception_ptr::exception_ptr(const exception_ptr& __other) _GLIBCXX_NOEXCEPT
    : _M_exception_object(__other._M_exception_object)
    {
      if (_M_exception_object)
	_M_addref();
    }

    _GLIBCXX_EH_PTR_USED
    inline
    exception_ptr::~exception_ptr() _GLIBCXX_USE_NOEXCEPT
    {
      if (_M_exception_object)
	_M_release();
    }

    _GLIBCXX_EH_PTR_USED
    inline exception_ptr&
    exception_ptr::operator=(const exception_ptr& __other) _GLIBCXX_USE_NOEXCEPT
    {
      exception_ptr(__other).swap(*this);
      return *this;
    }

    _GLIBCXX_EH_PTR_USED
    inline void
    exception_ptr::swap(exception_ptr &__other) _GLIBCXX_USE_NOEXCEPT
    {
      void *__tmp = _M_exception_object;
      _M_exception_object = __other._M_exception_object;
      __other._M_exception_object = __tmp;
    }

    /// @relates exception_ptr
    inline void
    swap(exception_ptr& __lhs, exception_ptr& __rhs)
    { __lhs.swap(__rhs); }

    /// @cond undocumented
    template<typename _Ex>
      inline void
      __dest_thunk(void* __x)
      { static_cast<_Ex*>(__x)->~_Ex(); }
    /// @endcond

  } // namespace __exception_ptr

  /// Obtain an exception_ptr pointing to a copy of the supplied object.
  template<typename _Ex>
    exception_ptr 
    make_exception_ptr(_Ex __ex) _GLIBCXX_USE_NOEXCEPT
    {

      void* __e = __cxxabiv1::__cxa_allocate_exception(sizeof(_Ex));
      (void) __cxxabiv1::__cxa_init_primary_exception(
	  __e, const_cast<std::type_info*>(&typeid(__ex)),
	  __exception_ptr::__dest_thunk<_Ex>);
      try
	{
          ::new (__e) _Ex(__ex);
          return exception_ptr(__e);
	}
      catch(...)
	{
	  __cxxabiv1::__cxa_free_exception(__e);
	  return current_exception();
	}
    }

#undef _GLIBCXX_EH_PTR_USED

  // @} group exceptions
} // namespace std

} // extern "C++"

#pragma GCC visibility pop

// Nested Exception support header (nested_exception class) for -*- C++ -*-

// Copyright (C) 2009-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/nested_exception.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{exception}
 */


#define _GLIBCXX_NESTED_EXCEPTION_H 1

#pragma GCC visibility push(default)






// Move, forward and identity for C++11 + swap -*- C++ -*-

// Copyright (C) 2007-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/move.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{utility}
 */


#define _MOVE_H 1






namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Used, in C++03 mode too, by allocators, etc.
  /**
   *  @brief Same as C++11 std::addressof
   *  @ingroup utilities
   */
  template<typename _Tp>
    inline _GLIBCXX_CONSTEXPR _Tp*
    __addressof(_Tp& __r) _GLIBCXX_NOEXCEPT
    { return __builtin_addressof(__r); }



_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// C++11 <type_traits> -*- C++ -*-

// Copyright (C) 2007-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/type_traits
 *  This is a Standard C++ Library header.
 */


#define _GLIBCXX_TYPE_TRAITS 1

       







namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   * @defgroup metaprogramming Metaprogramming
   * @ingroup utilities
   *
   * Template utilities for compile-time introspection and modification,
   * including type classification traits, type property inspection traits
   * and type transformation traits.
   *
   * @{
   */

  /// integral_constant
  template<typename _Tp, _Tp __v>
    struct integral_constant
    {
      static constexpr _Tp                  value = __v;
      typedef _Tp                           value_type;
      typedef integral_constant<_Tp, __v>   type;
      constexpr operator value_type() const noexcept { return value; }


#define __cpp_lib_integral_constant_callable 201304

      constexpr value_type operator()() const noexcept { return value; }

    };

  template<typename _Tp, _Tp __v>
    constexpr _Tp integral_constant<_Tp, __v>::value;

  /// The type used as a compile-time boolean with true value.
  typedef integral_constant<bool, true>     true_type;

  /// The type used as a compile-time boolean with false value.
  typedef integral_constant<bool, false>    false_type;

  template<bool __v>
    using __bool_constant = integral_constant<bool, __v>;


#define __cpp_lib_bool_constant 201505
  template<bool __v>
    using bool_constant = integral_constant<bool, __v>;


  // Meta programming helper types.

  template<bool, typename, typename>
    struct conditional;

  template <typename _Type>
    struct __type_identity
    { using type = _Type; };

  template<typename _Tp>
    using __type_identity_t = typename __type_identity<_Tp>::type;

  template<typename...>
    struct __or_;

  template<>
    struct __or_<>
    : public false_type
    { };

  template<typename _B1>
    struct __or_<_B1>
    : public _B1
    { };

  template<typename _B1, typename _B2>
    struct __or_<_B1, _B2>
    : public conditional<_B1::value, _B1, _B2>::type
    { };

  template<typename _B1, typename _B2, typename _B3, typename... _Bn>
    struct __or_<_B1, _B2, _B3, _Bn...>
    : public conditional<_B1::value, _B1, __or_<_B2, _B3, _Bn...>>::type
    { };

  template<typename...>
    struct __and_;

  template<>
    struct __and_<>
    : public true_type
    { };

  template<typename _B1>
    struct __and_<_B1>
    : public _B1
    { };

  template<typename _B1, typename _B2>
    struct __and_<_B1, _B2>
    : public conditional<_B1::value, _B2, _B1>::type
    { };

  template<typename _B1, typename _B2, typename _B3, typename... _Bn>
    struct __and_<_B1, _B2, _B3, _Bn...>
    : public conditional<_B1::value, __and_<_B2, _B3, _Bn...>, _B1>::type
    { };

  template<typename _Pp>
    struct __not_
    : public __bool_constant<!bool(_Pp::value)>
    { };



  template<typename... _Bn>
    inline constexpr bool __or_v = __or_<_Bn...>::value;
  template<typename... _Bn>
    inline constexpr bool __and_v = __and_<_Bn...>::value;

#define __cpp_lib_logical_traits 201510

  template<typename... _Bn>
    struct conjunction
    : __and_<_Bn...>
    { };

  template<typename... _Bn>
    struct disjunction
    : __or_<_Bn...>
    { };

  template<typename _Pp>
    struct negation
    : __not_<_Pp>
    { };

  template<typename... _Bn>
    inline constexpr bool conjunction_v = conjunction<_Bn...>::value;

  template<typename... _Bn>
    inline constexpr bool disjunction_v = disjunction<_Bn...>::value;

  template<typename _Pp>
    inline constexpr bool negation_v = negation<_Pp>::value;



  // Forward declarations
  template<typename>
    struct is_reference;
  template<typename>
    struct is_function;
  template<typename>
    struct is_void;
  template<typename>
    struct __is_array_unknown_bounds;

  // Helper functions that return false_type for incomplete classes,
  // incomplete unions and arrays of known bound from those.

  template <typename _Tp, size_t = sizeof(_Tp)>
    constexpr true_type __is_complete_or_unbounded(__type_identity<_Tp>)
    { return {}; }

  template <typename _TypeIdentity,
      typename _NestedType = typename _TypeIdentity::type>
    constexpr typename __or_<
      is_reference<_NestedType>,
      is_function<_NestedType>,
      is_void<_NestedType>,
      __is_array_unknown_bounds<_NestedType>
    >::type __is_complete_or_unbounded(_TypeIdentity)
    { return {}; }

  // For several sfinae-friendly trait implementations we transport both the
  // result information (as the member type) and the failure information (no
  // member type). This is very similar to std::enable_if, but we cannot use
  // them, because we need to derive from them as an implementation detail.

  template<typename _Tp>
    struct __success_type
    { typedef _Tp type; };

  struct __failure_type
  { };

  template<typename>
    struct remove_cv;

  // __remove_cv_t (std::remove_cv_t for C++11).
  template<typename _Tp>
    using __remove_cv_t = typename remove_cv<_Tp>::type;

  template<typename>
    struct is_const;

  // Primary type categories.

  template<typename>
    struct __is_void_helper
    : public false_type { };

  template<>
    struct __is_void_helper<void>
    : public true_type { };

  /// is_void
  template<typename _Tp>
    struct is_void
    : public __is_void_helper<__remove_cv_t<_Tp>>::type
    { };

  template<typename>
    struct __is_integral_helper
    : public false_type { };

  template<>
    struct __is_integral_helper<bool>
    : public true_type { };

  template<>
    struct __is_integral_helper<char>
    : public true_type { };

  template<>
    struct __is_integral_helper<signed char>
    : public true_type { };

  template<>
    struct __is_integral_helper<unsigned char>
    : public true_type { };

  // We want is_integral<wchar_t> to be true (and make_signed/unsigned to work)
  // even when libc doesn't provide working <wchar.h> and related functions,
  // so check __WCHAR_TYPE__ instead of _GLIBCXX_USE_WCHAR_T.

  template<>
    struct __is_integral_helper<wchar_t>
    : public true_type { };








  template<>
    struct __is_integral_helper<char16_t>
    : public true_type { };

  template<>
    struct __is_integral_helper<char32_t>
    : public true_type { };

  template<>
    struct __is_integral_helper<short>
    : public true_type { };

  template<>
    struct __is_integral_helper<unsigned short>
    : public true_type { };

  template<>
    struct __is_integral_helper<int>
    : public true_type { };

  template<>
    struct __is_integral_helper<unsigned int>
    : public true_type { };

  template<>
    struct __is_integral_helper<long>
    : public true_type { };

  template<>
    struct __is_integral_helper<unsigned long>
    : public true_type { };

  template<>
    struct __is_integral_helper<long long>
    : public true_type { };

  template<>
    struct __is_integral_helper<unsigned long long>
    : public true_type { };

  // Conditionalizing on __STRICT_ANSI__ here will break any port that
  // uses one of these types for size_t.

  /// is_integral
  template<typename _Tp>
    struct is_integral
    : public __is_integral_helper<__remove_cv_t<_Tp>>::type
    { };

  template<typename>
    struct __is_floating_point_helper
    : public false_type { };

  template<>
    struct __is_floating_point_helper<float>
    : public true_type { };

  template<>
    struct __is_floating_point_helper<double>
    : public true_type { };

  template<>
    struct __is_floating_point_helper<long double>
    : public true_type { };







  /// is_floating_point
  template<typename _Tp>
    struct is_floating_point
    : public __is_floating_point_helper<__remove_cv_t<_Tp>>::type
    { };

  /// is_array
  template<typename>
    struct is_array
    : public false_type { };

  template<typename _Tp, std::size_t _Size>
    struct is_array<_Tp[_Size]>
    : public true_type { };

  template<typename _Tp>
    struct is_array<_Tp[]>
    : public true_type { };

  template<typename>
    struct __is_pointer_helper
    : public false_type { };

  template<typename _Tp>
    struct __is_pointer_helper<_Tp*>
    : public true_type { };

  /// is_pointer
  template<typename _Tp>
    struct is_pointer
    : public __is_pointer_helper<__remove_cv_t<_Tp>>::type
    { };

  /// is_lvalue_reference
  template<typename>
    struct is_lvalue_reference
    : public false_type { };

  template<typename _Tp>
    struct is_lvalue_reference<_Tp&>
    : public true_type { };

  /// is_rvalue_reference
  template<typename>
    struct is_rvalue_reference
    : public false_type { };

  template<typename _Tp>
    struct is_rvalue_reference<_Tp&&>
    : public true_type { };

  template<typename>
    struct __is_member_object_pointer_helper
    : public false_type { };

  template<typename _Tp, typename _Cp>
    struct __is_member_object_pointer_helper<_Tp _Cp::*>
    : public __not_<is_function<_Tp>>::type { };

  /// is_member_object_pointer
  template<typename _Tp>
    struct is_member_object_pointer
    : public __is_member_object_pointer_helper<__remove_cv_t<_Tp>>::type
    { };

  template<typename>
    struct __is_member_function_pointer_helper
    : public false_type { };

  template<typename _Tp, typename _Cp>
    struct __is_member_function_pointer_helper<_Tp _Cp::*>
    : public is_function<_Tp>::type { };

  /// is_member_function_pointer
  template<typename _Tp>
    struct is_member_function_pointer
    : public __is_member_function_pointer_helper<__remove_cv_t<_Tp>>::type
    { };

  /// is_enum
  template<typename _Tp>
    struct is_enum
    : public integral_constant<bool, __is_enum(_Tp)>
    { };

  /// is_union
  template<typename _Tp>
    struct is_union
    : public integral_constant<bool, __is_union(_Tp)>
    { };

  /// is_class
  template<typename _Tp>
    struct is_class
    : public integral_constant<bool, __is_class(_Tp)>
    { };

  /// is_function
  template<typename _Tp>
    struct is_function
    : public __bool_constant<!is_const<const _Tp>::value> { };

  template<typename _Tp>
    struct is_function<_Tp&>
    : public false_type { };

  template<typename _Tp>
    struct is_function<_Tp&&>
    : public false_type { };

#define __cpp_lib_is_null_pointer 201309

  template<typename>
    struct __is_null_pointer_helper
    : public false_type { };

  template<>
    struct __is_null_pointer_helper<std::nullptr_t>
    : public true_type { };

  /// is_null_pointer (LWG 2247).
  template<typename _Tp>
    struct is_null_pointer
    : public __is_null_pointer_helper<__remove_cv_t<_Tp>>::type
    { };

  /// __is_nullptr_t (deprecated extension).
  template<typename _Tp>
    struct __is_nullptr_t
    : public is_null_pointer<_Tp>
    { } _GLIBCXX_DEPRECATED_SUGGEST("std::is_null_pointer");

  // Composite type categories.

  /// is_reference
  template<typename _Tp>
    struct is_reference
    : public __or_<is_lvalue_reference<_Tp>,
                   is_rvalue_reference<_Tp>>::type
    { };

  /// is_arithmetic
  template<typename _Tp>
    struct is_arithmetic
    : public __or_<is_integral<_Tp>, is_floating_point<_Tp>>::type
    { };

  /// is_fundamental
  template<typename _Tp>
    struct is_fundamental
    : public __or_<is_arithmetic<_Tp>, is_void<_Tp>,
		   is_null_pointer<_Tp>>::type
    { };

  /// is_object
  template<typename _Tp>
    struct is_object
    : public __not_<__or_<is_function<_Tp>, is_reference<_Tp>,
                          is_void<_Tp>>>::type
    { };

  template<typename>
    struct is_member_pointer;

  /// is_scalar
  template<typename _Tp>
    struct is_scalar
    : public __or_<is_arithmetic<_Tp>, is_enum<_Tp>, is_pointer<_Tp>,
                   is_member_pointer<_Tp>, is_null_pointer<_Tp>>::type
    { };

  /// is_compound
  template<typename _Tp>
    struct is_compound
    : public __not_<is_fundamental<_Tp>>::type { };

  template<typename _Tp>
    struct __is_member_pointer_helper
    : public false_type { };

  template<typename _Tp, typename _Cp>
    struct __is_member_pointer_helper<_Tp _Cp::*>
    : public true_type { };

  /// is_member_pointer
  template<typename _Tp>
    struct is_member_pointer
    : public __is_member_pointer_helper<__remove_cv_t<_Tp>>::type
    { };

  template<typename, typename>
    struct is_same;

  template<typename _Tp, typename... _Types>
    using __is_one_of = __or_<is_same<_Tp, _Types>...>;

  // Check if a type is one of the signed integer types.
  template<typename _Tp>
    using __is_signed_integer = __is_one_of<__remove_cv_t<_Tp>,
	  signed char, signed short, signed int, signed long,
	  signed long long












	  >;

  // Check if a type is one of the unsigned integer types.
  template<typename _Tp>
    using __is_unsigned_integer = __is_one_of<__remove_cv_t<_Tp>,
	  unsigned char, unsigned short, unsigned int, unsigned long,
	  unsigned long long












	  >;

  // Check if a type is one of the signed or unsigned integer types.
  template<typename _Tp>
    using __is_standard_integer
      = __or_<__is_signed_integer<_Tp>, __is_unsigned_integer<_Tp>>;

  // __void_t (std::void_t for C++11)
  template<typename...> using __void_t = void;

  // Utility to detect referenceable types ([defns.referenceable]).

  template<typename _Tp, typename = void>
    struct __is_referenceable
    : public false_type
    { };

  template<typename _Tp>
    struct __is_referenceable<_Tp, __void_t<_Tp&>>
    : public true_type
    { };

  // Type properties.

  /// is_const
  template<typename>
    struct is_const
    : public false_type { };

  template<typename _Tp>
    struct is_const<_Tp const>
    : public true_type { };

  /// is_volatile
  template<typename>
    struct is_volatile
    : public false_type { };

  template<typename _Tp>
    struct is_volatile<_Tp volatile>
    : public true_type { };

  /// is_trivial
  template<typename _Tp>
    struct is_trivial
    : public integral_constant<bool, __is_trivial(_Tp)>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  // is_trivially_copyable
  template<typename _Tp>
    struct is_trivially_copyable
    : public integral_constant<bool, __is_trivially_copyable(_Tp)>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// is_standard_layout
  template<typename _Tp>
    struct is_standard_layout
    : public integral_constant<bool, __is_standard_layout(_Tp)>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// is_pod (deprecated in C++20)
  // Could use is_standard_layout && is_trivial instead of the builtin.
  template<typename _Tp>
    struct
    _GLIBCXX20_DEPRECATED("use is_standard_layout && is_trivial instead")
    is_pod
    : public integral_constant<bool, __is_pod(_Tp)>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// is_literal_type
  template<typename _Tp>
    struct
    _GLIBCXX17_DEPRECATED
    is_literal_type
    : public integral_constant<bool, __is_literal_type(_Tp)>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// is_empty
  template<typename _Tp>
    struct is_empty
    : public integral_constant<bool, __is_empty(_Tp)>
    { };

  /// is_polymorphic
  template<typename _Tp>
    struct is_polymorphic
    : public integral_constant<bool, __is_polymorphic(_Tp)>
    { };


#define __cpp_lib_is_final 201402L
  /// is_final
  template<typename _Tp>
    struct is_final
    : public integral_constant<bool, __is_final(_Tp)>
    { };


  /// is_abstract
  template<typename _Tp>
    struct is_abstract
    : public integral_constant<bool, __is_abstract(_Tp)>
    { };

  template<typename _Tp,
	   bool = is_arithmetic<_Tp>::value>
    struct __is_signed_helper
    : public false_type { };

  template<typename _Tp>
    struct __is_signed_helper<_Tp, true>
    : public integral_constant<bool, _Tp(-1) < _Tp(0)>
    { };

  /// is_signed
  template<typename _Tp>
    struct is_signed
    : public __is_signed_helper<_Tp>::type
    { };

  /// is_unsigned
  template<typename _Tp>
    struct is_unsigned
    : public __and_<is_arithmetic<_Tp>, __not_<is_signed<_Tp>>>
    { };


  // Destructible and constructible type properties.

  /**
   *  @brief  Utility to simplify expressions used in unevaluated operands
   *  @ingroup utilities
   */

  template<typename _Tp, typename _Up = _Tp&&>
    _Up
    __declval(int);

  template<typename _Tp>
    _Tp
    __declval(long);

  template<typename _Tp>
    auto declval() noexcept -> decltype(__declval<_Tp>(0));

  template<typename, unsigned = 0>
    struct extent;

  template<typename>
    struct remove_all_extents;

  template<typename _Tp>
    struct __is_array_known_bounds
    : public integral_constant<bool, (extent<_Tp>::value > 0)>
    { };

  template<typename _Tp>
    struct __is_array_unknown_bounds
    : public __and_<is_array<_Tp>, __not_<extent<_Tp>>>
    { };

  // In N3290 is_destructible does not say anything about function
  // types and abstract types, see LWG 2049. This implementation
  // describes function types as non-destructible and all complete
  // object types as destructible, iff the explicit destructor
  // call expression is wellformed.
  struct __do_is_destructible_impl
  {
    template<typename _Tp, typename = decltype(declval<_Tp&>().~_Tp())>
      static true_type __test(int);

    template<typename>
      static false_type __test(...);
  };

  template<typename _Tp>
    struct __is_destructible_impl
    : public __do_is_destructible_impl
    {
      typedef decltype(__test<_Tp>(0)) type;
    };

  template<typename _Tp,
           bool = __or_<is_void<_Tp>,
                        __is_array_unknown_bounds<_Tp>,
                        is_function<_Tp>>::value,
           bool = __or_<is_reference<_Tp>, is_scalar<_Tp>>::value>
    struct __is_destructible_safe;

  template<typename _Tp>
    struct __is_destructible_safe<_Tp, false, false>
    : public __is_destructible_impl<typename
               remove_all_extents<_Tp>::type>::type
    { };

  template<typename _Tp>
    struct __is_destructible_safe<_Tp, true, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_destructible_safe<_Tp, false, true>
    : public true_type { };

  /// is_destructible
  template<typename _Tp>
    struct is_destructible
    : public __is_destructible_safe<_Tp>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  // is_nothrow_destructible requires that is_destructible is
  // satisfied as well.  We realize that by mimicing the
  // implementation of is_destructible but refer to noexcept(expr)
  // instead of decltype(expr).
  struct __do_is_nt_destructible_impl
  {
    template<typename _Tp>
      static __bool_constant<noexcept(declval<_Tp&>().~_Tp())>
      __test(int);

    template<typename>
      static false_type __test(...);
  };

  template<typename _Tp>
    struct __is_nt_destructible_impl
    : public __do_is_nt_destructible_impl
    {
      typedef decltype(__test<_Tp>(0)) type;
    };

  template<typename _Tp,
           bool = __or_<is_void<_Tp>,
                        __is_array_unknown_bounds<_Tp>,
                        is_function<_Tp>>::value,
           bool = __or_<is_reference<_Tp>, is_scalar<_Tp>>::value>
    struct __is_nt_destructible_safe;

  template<typename _Tp>
    struct __is_nt_destructible_safe<_Tp, false, false>
    : public __is_nt_destructible_impl<typename
               remove_all_extents<_Tp>::type>::type
    { };

  template<typename _Tp>
    struct __is_nt_destructible_safe<_Tp, true, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_nt_destructible_safe<_Tp, false, true>
    : public true_type { };

  /// is_nothrow_destructible
  template<typename _Tp>
    struct is_nothrow_destructible
    : public __is_nt_destructible_safe<_Tp>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, typename... _Args>
    struct __is_constructible_impl
    : public __bool_constant<__is_constructible(_Tp, _Args...)>
    { };

  /// is_constructible
  template<typename _Tp, typename... _Args>
    struct is_constructible
      : public __is_constructible_impl<_Tp, _Args...>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// is_default_constructible
  template<typename _Tp>
    struct is_default_constructible
    : public __is_constructible_impl<_Tp>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __is_copy_constructible_impl;

  template<typename _Tp>
    struct __is_copy_constructible_impl<_Tp, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_copy_constructible_impl<_Tp, true>
    : public __is_constructible_impl<_Tp, const _Tp&>
    { };

  /// is_copy_constructible
  template<typename _Tp>
    struct is_copy_constructible
    : public __is_copy_constructible_impl<_Tp>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __is_move_constructible_impl;

  template<typename _Tp>
    struct __is_move_constructible_impl<_Tp, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_move_constructible_impl<_Tp, true>
    : public __is_constructible_impl<_Tp, _Tp&&>
    { };

  /// is_move_constructible
  template<typename _Tp>
    struct is_move_constructible
    : public __is_move_constructible_impl<_Tp>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, typename... _Args>
    using __is_nothrow_constructible_impl
      = __bool_constant<__is_nothrow_constructible(_Tp, _Args...)>;

  /// is_nothrow_constructible
  template<typename _Tp, typename... _Args>
    struct is_nothrow_constructible
    : public __is_nothrow_constructible_impl<_Tp, _Args...>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// is_nothrow_default_constructible
  template<typename _Tp>
    struct is_nothrow_default_constructible
    : public __bool_constant<__is_nothrow_constructible(_Tp)>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };


  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __is_nothrow_copy_constructible_impl;

  template<typename _Tp>
    struct __is_nothrow_copy_constructible_impl<_Tp, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_nothrow_copy_constructible_impl<_Tp, true>
    : public __is_nothrow_constructible_impl<_Tp, const _Tp&>
    { };

  /// is_nothrow_copy_constructible
  template<typename _Tp>
    struct is_nothrow_copy_constructible
    : public __is_nothrow_copy_constructible_impl<_Tp>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __is_nothrow_move_constructible_impl;

  template<typename _Tp>
    struct __is_nothrow_move_constructible_impl<_Tp, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_nothrow_move_constructible_impl<_Tp, true>
    : public __is_nothrow_constructible_impl<_Tp, _Tp&&>
    { };

  /// is_nothrow_move_constructible
  template<typename _Tp>
    struct is_nothrow_move_constructible
    : public __is_nothrow_move_constructible_impl<_Tp>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// is_assignable
  template<typename _Tp, typename _Up>
    struct is_assignable
    : public __bool_constant<__is_assignable(_Tp, _Up)>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __is_copy_assignable_impl;

  template<typename _Tp>
    struct __is_copy_assignable_impl<_Tp, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_copy_assignable_impl<_Tp, true>
    : public __bool_constant<__is_assignable(_Tp&, const _Tp&)>
    { };

  /// is_copy_assignable
  template<typename _Tp>
    struct is_copy_assignable
    : public __is_copy_assignable_impl<_Tp>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __is_move_assignable_impl;

  template<typename _Tp>
    struct __is_move_assignable_impl<_Tp, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_move_assignable_impl<_Tp, true>
    : public __bool_constant<__is_assignable(_Tp&, _Tp&&)>
    { };

  /// is_move_assignable
  template<typename _Tp>
    struct is_move_assignable
    : public __is_move_assignable_impl<_Tp>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, typename _Up>
    using __is_nothrow_assignable_impl
      = __bool_constant<__is_nothrow_assignable(_Tp, _Up)>;

  /// is_nothrow_assignable
  template<typename _Tp, typename _Up>
    struct is_nothrow_assignable
    : public __is_nothrow_assignable_impl<_Tp, _Up>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __is_nt_copy_assignable_impl;

  template<typename _Tp>
    struct __is_nt_copy_assignable_impl<_Tp, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_nt_copy_assignable_impl<_Tp, true>
    : public __is_nothrow_assignable_impl<_Tp&, const _Tp&>
    { };

  /// is_nothrow_copy_assignable
  template<typename _Tp>
    struct is_nothrow_copy_assignable
    : public __is_nt_copy_assignable_impl<_Tp>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __is_nt_move_assignable_impl;

  template<typename _Tp>
    struct __is_nt_move_assignable_impl<_Tp, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_nt_move_assignable_impl<_Tp, true>
    : public __is_nothrow_assignable_impl<_Tp&, _Tp&&>
    { };

  /// is_nothrow_move_assignable
  template<typename _Tp>
    struct is_nothrow_move_assignable
    : public __is_nt_move_assignable_impl<_Tp>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// is_trivially_constructible
  template<typename _Tp, typename... _Args>
    struct is_trivially_constructible
    : public __bool_constant<__is_trivially_constructible(_Tp, _Args...)>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// is_trivially_default_constructible
  template<typename _Tp>
    struct is_trivially_default_constructible
    : public __bool_constant<__is_trivially_constructible(_Tp)>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  struct __do_is_implicitly_default_constructible_impl
  {
    template <typename _Tp>
    static void __helper(const _Tp&);

    template <typename _Tp>
    static true_type __test(const _Tp&,
                            decltype(__helper<const _Tp&>({}))* = 0);

    static false_type __test(...);
  };

  template<typename _Tp>
    struct __is_implicitly_default_constructible_impl
    : public __do_is_implicitly_default_constructible_impl
    {
      typedef decltype(__test(declval<_Tp>())) type;
    };

  template<typename _Tp>
    struct __is_implicitly_default_constructible_safe
    : public __is_implicitly_default_constructible_impl<_Tp>::type
    { };

  template <typename _Tp>
    struct __is_implicitly_default_constructible
    : public __and_<__is_constructible_impl<_Tp>,
		    __is_implicitly_default_constructible_safe<_Tp>>
    { };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __is_trivially_copy_constructible_impl;

  template<typename _Tp>
    struct __is_trivially_copy_constructible_impl<_Tp, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_trivially_copy_constructible_impl<_Tp, true>
    : public __and_<__is_copy_constructible_impl<_Tp>,
		    integral_constant<bool,
			__is_trivially_constructible(_Tp, const _Tp&)>>
    { };

  /// is_trivially_copy_constructible
  template<typename _Tp>
    struct is_trivially_copy_constructible
    : public __is_trivially_copy_constructible_impl<_Tp>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __is_trivially_move_constructible_impl;

  template<typename _Tp>
    struct __is_trivially_move_constructible_impl<_Tp, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_trivially_move_constructible_impl<_Tp, true>
    : public __and_<__is_move_constructible_impl<_Tp>,
		    integral_constant<bool,
			__is_trivially_constructible(_Tp, _Tp&&)>>
    { };

  /// is_trivially_move_constructible
  template<typename _Tp>
    struct is_trivially_move_constructible
    : public __is_trivially_move_constructible_impl<_Tp>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// is_trivially_assignable
  template<typename _Tp, typename _Up>
    struct is_trivially_assignable
    : public __bool_constant<__is_trivially_assignable(_Tp, _Up)>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __is_trivially_copy_assignable_impl;

  template<typename _Tp>
    struct __is_trivially_copy_assignable_impl<_Tp, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_trivially_copy_assignable_impl<_Tp, true>
    : public __bool_constant<__is_trivially_assignable(_Tp&, const _Tp&)>
    { };

  /// is_trivially_copy_assignable
  template<typename _Tp>
    struct is_trivially_copy_assignable
    : public __is_trivially_copy_assignable_impl<_Tp>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __is_trivially_move_assignable_impl;

  template<typename _Tp>
    struct __is_trivially_move_assignable_impl<_Tp, false>
    : public false_type { };

  template<typename _Tp>
    struct __is_trivially_move_assignable_impl<_Tp, true>
    : public __bool_constant<__is_trivially_assignable(_Tp&, _Tp&&)>
    { };

  /// is_trivially_move_assignable
  template<typename _Tp>
    struct is_trivially_move_assignable
    : public __is_trivially_move_assignable_impl<_Tp>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// is_trivially_destructible
  template<typename _Tp>
    struct is_trivially_destructible
    : public __and_<__is_destructible_safe<_Tp>,
		    __bool_constant<__has_trivial_destructor(_Tp)>>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };


  /// has_virtual_destructor
  template<typename _Tp>
    struct has_virtual_destructor
    : public integral_constant<bool, __has_virtual_destructor(_Tp)>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };


  // type property queries.

  /// alignment_of
  template<typename _Tp>
    struct alignment_of
    : public integral_constant<std::size_t, alignof(_Tp)>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// rank
  template<typename>
    struct rank
    : public integral_constant<std::size_t, 0> { };

  template<typename _Tp, std::size_t _Size>
    struct rank<_Tp[_Size]>
    : public integral_constant<std::size_t, 1 + rank<_Tp>::value> { };

  template<typename _Tp>
    struct rank<_Tp[]>
    : public integral_constant<std::size_t, 1 + rank<_Tp>::value> { };

  /// extent
  template<typename, unsigned _Uint>
    struct extent
    : public integral_constant<std::size_t, 0> { };

  template<typename _Tp, unsigned _Uint, std::size_t _Size>
    struct extent<_Tp[_Size], _Uint>
    : public integral_constant<std::size_t,
			       _Uint == 0 ? _Size : extent<_Tp,
							   _Uint - 1>::value>
    { };

  template<typename _Tp, unsigned _Uint>
    struct extent<_Tp[], _Uint>
    : public integral_constant<std::size_t,
			       _Uint == 0 ? 0 : extent<_Tp,
						       _Uint - 1>::value>
    { };


  // Type relations.

  /// is_same
  template<typename _Tp, typename _Up>
    struct is_same

    : public integral_constant<bool, __is_same(_Tp, _Up)>



    { };








  /// is_base_of
  template<typename _Base, typename _Derived>
    struct is_base_of
    : public integral_constant<bool, __is_base_of(_Base, _Derived)>
    { };

  template<typename _From, typename _To,
           bool = __or_<is_void<_From>, is_function<_To>,
                        is_array<_To>>::value>
    struct __is_convertible_helper
    {
      typedef typename is_void<_To>::type type;
    };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wctor-dtor-privacy"
  template<typename _From, typename _To>
    class __is_convertible_helper<_From, _To, false>
    {
      template<typename _To1>
	static void __test_aux(_To1) noexcept;

      template<typename _From1, typename _To1,
	       typename = decltype(__test_aux<_To1>(std::declval<_From1>()))>
	static true_type
	__test(int);

      template<typename, typename>
	static false_type
	__test(...);

    public:
      typedef decltype(__test<_From, _To>(0)) type;
    };
#pragma GCC diagnostic pop

  /// is_convertible
  template<typename _From, typename _To>
    struct is_convertible
    : public __is_convertible_helper<_From, _To>::type
    { };

  // helper trait for unique_ptr<T[]>, shared_ptr<T[]>, and span<T, N>
  template<typename _ToElementType, typename _FromElementType>
    using __is_array_convertible
      = is_convertible<_FromElementType(*)[], _ToElementType(*)[]>;

  template<typename _From, typename _To,
           bool = __or_<is_void<_From>, is_function<_To>,
                        is_array<_To>>::value>
    struct __is_nt_convertible_helper
    : is_void<_To>
    { };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wctor-dtor-privacy"
  template<typename _From, typename _To>
    class __is_nt_convertible_helper<_From, _To, false>
    {
      template<typename _To1>
	static void __test_aux(_To1) noexcept;

      template<typename _From1, typename _To1>
	static
	__bool_constant<noexcept(__test_aux<_To1>(std::declval<_From1>()))>
	__test(int);

      template<typename, typename>
	static false_type
	__test(...);

    public:
      using type = decltype(__test<_From, _To>(0));
    };
#pragma GCC diagnostic pop

  // is_nothrow_convertible for C++11
  template<typename _From, typename _To>
    struct __is_nothrow_convertible
    : public __is_nt_convertible_helper<_From, _To>::type
    { };


  // Const-volatile modifications.

  /// remove_const
  template<typename _Tp>
    struct remove_const
    { typedef _Tp     type; };

  template<typename _Tp>
    struct remove_const<_Tp const>
    { typedef _Tp     type; };

  /// remove_volatile
  template<typename _Tp>
    struct remove_volatile
    { typedef _Tp     type; };

  template<typename _Tp>
    struct remove_volatile<_Tp volatile>
    { typedef _Tp     type; };

  /// remove_cv
  template<typename _Tp>
    struct remove_cv
    { using type = _Tp; };

  template<typename _Tp>
    struct remove_cv<const _Tp>
    { using type = _Tp; };

  template<typename _Tp>
    struct remove_cv<volatile _Tp>
    { using type = _Tp; };

  template<typename _Tp>
    struct remove_cv<const volatile _Tp>
    { using type = _Tp; };

  /// add_const
  template<typename _Tp>
    struct add_const
    { typedef _Tp const     type; };

  /// add_volatile
  template<typename _Tp>
    struct add_volatile
    { typedef _Tp volatile     type; };

  /// add_cv
  template<typename _Tp>
    struct add_cv
    {
      typedef typename
      add_const<typename add_volatile<_Tp>::type>::type     type;
    };



#define __cpp_lib_transformation_trait_aliases 201304

  /// Alias template for remove_const
  template<typename _Tp>
    using remove_const_t = typename remove_const<_Tp>::type;

  /// Alias template for remove_volatile
  template<typename _Tp>
    using remove_volatile_t = typename remove_volatile<_Tp>::type;

  /// Alias template for remove_cv
  template<typename _Tp>
    using remove_cv_t = typename remove_cv<_Tp>::type;

  /// Alias template for add_const
  template<typename _Tp>
    using add_const_t = typename add_const<_Tp>::type;

  /// Alias template for add_volatile
  template<typename _Tp>
    using add_volatile_t = typename add_volatile<_Tp>::type;

  /// Alias template for add_cv
  template<typename _Tp>
    using add_cv_t = typename add_cv<_Tp>::type;


  // Reference transformations.

  /// remove_reference
  template<typename _Tp>
    struct remove_reference
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&>
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&&>
    { typedef _Tp   type; };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __add_lvalue_reference_helper
    { typedef _Tp   type; };

  template<typename _Tp>
    struct __add_lvalue_reference_helper<_Tp, true>
    { typedef _Tp&   type; };

  /// add_lvalue_reference
  template<typename _Tp>
    struct add_lvalue_reference
    : public __add_lvalue_reference_helper<_Tp>
    { };

  template<typename _Tp, bool = __is_referenceable<_Tp>::value>
    struct __add_rvalue_reference_helper
    { typedef _Tp   type; };

  template<typename _Tp>
    struct __add_rvalue_reference_helper<_Tp, true>
    { typedef _Tp&&   type; };

  /// add_rvalue_reference
  template<typename _Tp>
    struct add_rvalue_reference
    : public __add_rvalue_reference_helper<_Tp>
    { };


  /// Alias template for remove_reference
  template<typename _Tp>
    using remove_reference_t = typename remove_reference<_Tp>::type;

  /// Alias template for add_lvalue_reference
  template<typename _Tp>
    using add_lvalue_reference_t = typename add_lvalue_reference<_Tp>::type;

  /// Alias template for add_rvalue_reference
  template<typename _Tp>
    using add_rvalue_reference_t = typename add_rvalue_reference<_Tp>::type;


  // Sign modifications.

  // Utility for constructing identically cv-qualified types.
  template<typename _Unqualified, bool _IsConst, bool _IsVol>
    struct __cv_selector;

  template<typename _Unqualified>
    struct __cv_selector<_Unqualified, false, false>
    { typedef _Unqualified __type; };

  template<typename _Unqualified>
    struct __cv_selector<_Unqualified, false, true>
    { typedef volatile _Unqualified __type; };

  template<typename _Unqualified>
    struct __cv_selector<_Unqualified, true, false>
    { typedef const _Unqualified __type; };

  template<typename _Unqualified>
    struct __cv_selector<_Unqualified, true, true>
    { typedef const volatile _Unqualified __type; };

  template<typename _Qualified, typename _Unqualified,
	   bool _IsConst = is_const<_Qualified>::value,
	   bool _IsVol = is_volatile<_Qualified>::value>
    class __match_cv_qualifiers
    {
      typedef __cv_selector<_Unqualified, _IsConst, _IsVol> __match;

    public:
      typedef typename __match::__type __type;
    };

  // Utility for finding the unsigned versions of signed integral types.
  template<typename _Tp>
    struct __make_unsigned
    { typedef _Tp __type; };

  template<>
    struct __make_unsigned<char>
    { typedef unsigned char __type; };

  template<>
    struct __make_unsigned<signed char>
    { typedef unsigned char __type; };

  template<>
    struct __make_unsigned<short>
    { typedef unsigned short __type; };

  template<>
    struct __make_unsigned<int>
    { typedef unsigned int __type; };

  template<>
    struct __make_unsigned<long>
    { typedef unsigned long __type; };

  template<>
    struct __make_unsigned<long long>
    { typedef unsigned long long __type; };






















  // Select between integral and enum: not possible to be both.
  template<typename _Tp,
	   bool _IsInt = is_integral<_Tp>::value,
	   bool _IsEnum = is_enum<_Tp>::value>
    class __make_unsigned_selector;

  template<typename _Tp>
    class __make_unsigned_selector<_Tp, true, false>
    {
      using __unsigned_type
	= typename __make_unsigned<__remove_cv_t<_Tp>>::__type;

    public:
      using __type
	= typename __match_cv_qualifiers<_Tp, __unsigned_type>::__type;
    };

  class __make_unsigned_selector_base
  {
  protected:
    template<typename...> struct _List { };

    template<typename _Tp, typename... _Up>
      struct _List<_Tp, _Up...> : _List<_Up...>
      { static constexpr size_t __size = sizeof(_Tp); };

    template<size_t _Sz, typename _Tp, bool = (_Sz <= _Tp::__size)>
      struct __select;

    template<size_t _Sz, typename _Uint, typename... _UInts>
      struct __select<_Sz, _List<_Uint, _UInts...>, true>
      { using __type = _Uint; };

    template<size_t _Sz, typename _Uint, typename... _UInts>
      struct __select<_Sz, _List<_Uint, _UInts...>, false>
      : __select<_Sz, _List<_UInts...>>
      { };
  };

  // Choose unsigned integer type with the smallest rank and same size as _Tp
  template<typename _Tp>
    class __make_unsigned_selector<_Tp, false, true>
    : __make_unsigned_selector_base
    {
      // With -fshort-enums, an enum may be as small as a char.
      using _UInts = _List<unsigned char, unsigned short, unsigned int,
			   unsigned long, unsigned long long>;

      using __unsigned_type = typename __select<sizeof(_Tp), _UInts>::__type;

    public:
      using __type
	= typename __match_cv_qualifiers<_Tp, __unsigned_type>::__type;
    };

  // wchar_t, char8_t, char16_t and char32_t are integral types but are
  // neither signed integer types nor unsigned integer types, so must be
  // transformed to the unsigned integer type with the smallest rank.
  // Use the partial specialization for enumeration types to do that.

  template<>
    struct __make_unsigned<wchar_t>
    {
      using __type
	= typename __make_unsigned_selector<wchar_t, false, true>::__type;
    };



  template<>
    struct __make_unsigned<char16_t>
    {
      using __type
	= typename __make_unsigned_selector<char16_t, false, true>::__type;
    };

  template<>
    struct __make_unsigned<char32_t>
    {
      using __type
	= typename __make_unsigned_selector<char32_t, false, true>::__type;
    };

  // Given an integral/enum type, return the corresponding unsigned
  // integer type.
  // Primary template.
  /// make_unsigned
  template<typename _Tp>
    struct make_unsigned
    { typedef typename __make_unsigned_selector<_Tp>::__type type; };

  // Integral, but don't define.
  template<>
    struct make_unsigned<bool>;


  // Utility for finding the signed versions of unsigned integral types.
  template<typename _Tp>
    struct __make_signed
    { typedef _Tp __type; };

  template<>
    struct __make_signed<char>
    { typedef signed char __type; };

  template<>
    struct __make_signed<unsigned char>
    { typedef signed char __type; };

  template<>
    struct __make_signed<unsigned short>
    { typedef signed short __type; };

  template<>
    struct __make_signed<unsigned int>
    { typedef signed int __type; };

  template<>
    struct __make_signed<unsigned long>
    { typedef signed long __type; };

  template<>
    struct __make_signed<unsigned long long>
    { typedef signed long long __type; };






















  // Select between integral and enum: not possible to be both.
  template<typename _Tp,
	   bool _IsInt = is_integral<_Tp>::value,
	   bool _IsEnum = is_enum<_Tp>::value>
    class __make_signed_selector;

  template<typename _Tp>
    class __make_signed_selector<_Tp, true, false>
    {
      using __signed_type
	= typename __make_signed<__remove_cv_t<_Tp>>::__type;

    public:
      using __type
	= typename __match_cv_qualifiers<_Tp, __signed_type>::__type;
    };

  // Choose signed integer type with the smallest rank and same size as _Tp
  template<typename _Tp>
    class __make_signed_selector<_Tp, false, true>
    {
      typedef typename __make_unsigned_selector<_Tp>::__type __unsigned_type;

    public:
      typedef typename __make_signed_selector<__unsigned_type>::__type __type;
    };

  // wchar_t, char16_t and char32_t are integral types but are neither
  // signed integer types nor unsigned integer types, so must be
  // transformed to the signed integer type with the smallest rank.
  // Use the partial specialization for enumeration types to do that.

  template<>
    struct __make_signed<wchar_t>
    {
      using __type
	= typename __make_signed_selector<wchar_t, false, true>::__type;
    };



  template<>
    struct __make_signed<char16_t>
    {
      using __type
	= typename __make_signed_selector<char16_t, false, true>::__type;
    };

  template<>
    struct __make_signed<char32_t>
    {
      using __type
	= typename __make_signed_selector<char32_t, false, true>::__type;
    };

  // Given an integral/enum type, return the corresponding signed
  // integer type.
  // Primary template.
  /// make_signed
  template<typename _Tp>
    struct make_signed
    { typedef typename __make_signed_selector<_Tp>::__type type; };

  // Integral, but don't define.
  template<>
    struct make_signed<bool>;


  /// Alias template for make_signed
  template<typename _Tp>
    using make_signed_t = typename make_signed<_Tp>::type;

  /// Alias template for make_unsigned
  template<typename _Tp>
    using make_unsigned_t = typename make_unsigned<_Tp>::type;


  // Array modifications.

  /// remove_extent
  template<typename _Tp>
    struct remove_extent
    { typedef _Tp     type; };

  template<typename _Tp, std::size_t _Size>
    struct remove_extent<_Tp[_Size]>
    { typedef _Tp     type; };

  template<typename _Tp>
    struct remove_extent<_Tp[]>
    { typedef _Tp     type; };

  /// remove_all_extents
  template<typename _Tp>
    struct remove_all_extents
    { typedef _Tp     type; };

  template<typename _Tp, std::size_t _Size>
    struct remove_all_extents<_Tp[_Size]>
    { typedef typename remove_all_extents<_Tp>::type     type; };

  template<typename _Tp>
    struct remove_all_extents<_Tp[]>
    { typedef typename remove_all_extents<_Tp>::type     type; };


  /// Alias template for remove_extent
  template<typename _Tp>
    using remove_extent_t = typename remove_extent<_Tp>::type;

  /// Alias template for remove_all_extents
  template<typename _Tp>
    using remove_all_extents_t = typename remove_all_extents<_Tp>::type;


  // Pointer modifications.

  template<typename _Tp, typename>
    struct __remove_pointer_helper
    { typedef _Tp     type; };

  template<typename _Tp, typename _Up>
    struct __remove_pointer_helper<_Tp, _Up*>
    { typedef _Up     type; };

  /// remove_pointer
  template<typename _Tp>
    struct remove_pointer
    : public __remove_pointer_helper<_Tp, __remove_cv_t<_Tp>>
    { };

  /// add_pointer
  template<typename _Tp, bool = __or_<__is_referenceable<_Tp>,
				      is_void<_Tp>>::value>
    struct __add_pointer_helper
    { typedef _Tp     type; };

  template<typename _Tp>
    struct __add_pointer_helper<_Tp, true>
    { typedef typename remove_reference<_Tp>::type*     type; };

  template<typename _Tp>
    struct add_pointer
    : public __add_pointer_helper<_Tp>
    { };


  /// Alias template for remove_pointer
  template<typename _Tp>
    using remove_pointer_t = typename remove_pointer<_Tp>::type;

  /// Alias template for add_pointer
  template<typename _Tp>
    using add_pointer_t = typename add_pointer<_Tp>::type;


  template<std::size_t _Len>
    struct __aligned_storage_msa
    {
      union __type
      {
	unsigned char __data[_Len];
	struct __attribute__((__aligned__)) { } __align;
      };
    };

  /**
   *  @brief Alignment type.
   *
   *  The value of _Align is a default-alignment which shall be the
   *  most stringent alignment requirement for any C++ object type
   *  whose size is no greater than _Len (3.9). The member typedef
   *  type shall be a POD type suitable for use as uninitialized
   *  storage for any object whose size is at most _Len and whose
   *  alignment is a divisor of _Align.
  */
  template<std::size_t _Len, std::size_t _Align =
	   __alignof__(typename __aligned_storage_msa<_Len>::__type)>
    struct aligned_storage
    {
      union type
      {
	unsigned char __data[_Len];
	struct __attribute__((__aligned__((_Align)))) { } __align;
      };
    };

  template <typename... _Types>
    struct __strictest_alignment
    {
      static const size_t _S_alignment = 0;
      static const size_t _S_size = 0;
    };

  template <typename _Tp, typename... _Types>
    struct __strictest_alignment<_Tp, _Types...>
    {
      static const size_t _S_alignment =
        alignof(_Tp) > __strictest_alignment<_Types...>::_S_alignment
	? alignof(_Tp) : __strictest_alignment<_Types...>::_S_alignment;
      static const size_t _S_size =
        sizeof(_Tp) > __strictest_alignment<_Types...>::_S_size
	? sizeof(_Tp) : __strictest_alignment<_Types...>::_S_size;
    };

  /**
   *  @brief Provide aligned storage for types.
   *
   *  [meta.trans.other]
   *
   *  Provides aligned storage for any of the provided types of at
   *  least size _Len.
   *
   *  @see aligned_storage
   */
  template <size_t _Len, typename... _Types>
    struct aligned_union
    {
    private:
      static_assert(sizeof...(_Types) != 0, "At least one type is required");

      using __strictest = __strictest_alignment<_Types...>;
      static const size_t _S_len = _Len > __strictest::_S_size
	? _Len : __strictest::_S_size;
    public:
      /// The value of the strictest alignment of _Types.
      static const size_t alignment_value = __strictest::_S_alignment;
      /// The storage.
      typedef typename aligned_storage<_S_len, alignment_value>::type type;
    };

  template <size_t _Len, typename... _Types>
    const size_t aligned_union<_Len, _Types...>::alignment_value;

  // Decay trait for arrays and functions, used for perfect forwarding
  // in make_pair, make_tuple, etc.
  template<typename _Up,
	   bool _IsArray = is_array<_Up>::value,
	   bool _IsFunction = is_function<_Up>::value>
    struct __decay_selector;

  // NB: DR 705.
  template<typename _Up>
    struct __decay_selector<_Up, false, false>
    { typedef __remove_cv_t<_Up> __type; };

  template<typename _Up>
    struct __decay_selector<_Up, true, false>
    { typedef typename remove_extent<_Up>::type* __type; };

  template<typename _Up>
    struct __decay_selector<_Up, false, true>
    { typedef typename add_pointer<_Up>::type __type; };

  /// decay
  template<typename _Tp>
    class decay
    {
      typedef typename remove_reference<_Tp>::type __remove_type;

    public:
      typedef typename __decay_selector<__remove_type>::__type type;
    };

  // __decay_t (std::decay_t for C++11).
  template<typename _Tp>
    using __decay_t = typename decay<_Tp>::type;

  template<typename _Tp>
    class reference_wrapper;

  // Helper which adds a reference to a type when given a reference_wrapper
  template<typename _Tp>
    struct __strip_reference_wrapper
    {
      typedef _Tp __type;
    };

  template<typename _Tp>
    struct __strip_reference_wrapper<reference_wrapper<_Tp> >
    {
      typedef _Tp& __type;
    };

  template<typename _Tp>
    using __decay_and_strip = __strip_reference_wrapper<__decay_t<_Tp>>;


  // Primary template.
  /// Define a member typedef @c type only if a boolean constant is true.
  template<bool, typename _Tp = void>
    struct enable_if
    { };

  // Partial specialization for true.
  template<typename _Tp>
    struct enable_if<true, _Tp>
    { typedef _Tp type; };

  // __enable_if_t (std::enable_if_t for C++11)
  template<bool _Cond, typename _Tp = void>
    using __enable_if_t = typename enable_if<_Cond, _Tp>::type;

  template<typename... _Cond>
    using _Require = __enable_if_t<__and_<_Cond...>::value>;

  // Primary template.
  /// Define a member typedef @c type to one of two argument types.
  template<bool _Cond, typename _Iftrue, typename _Iffalse>
    struct conditional
    { typedef _Iftrue type; };

  // Partial specialization for false.
  template<typename _Iftrue, typename _Iffalse>
    struct conditional<false, _Iftrue, _Iffalse>
    { typedef _Iffalse type; };

  // __remove_cvref_t (std::remove_cvref_t for C++11).
  template<typename _Tp>
    using __remove_cvref_t
     = typename remove_cv<typename remove_reference<_Tp>::type>::type;

  /// common_type
  template<typename... _Tp>
    struct common_type;

  // Sfinae-friendly common_type implementation:

  struct __do_common_type_impl
  {
    template<typename _Tp, typename _Up>
      using __cond_t
	= decltype(true ? std::declval<_Tp>() : std::declval<_Up>());

    // if decay_t<decltype(false ? declval<D1>() : declval<D2>())>
    // denotes a valid type, let C denote that type.
    template<typename _Tp, typename _Up>
      static __success_type<__decay_t<__cond_t<_Tp, _Up>>>
      _S_test(int);









    template<typename, typename>
      static __failure_type
      _S_test_2(...);

    template<typename _Tp, typename _Up>
      static decltype(_S_test_2<_Tp, _Up>(0))
      _S_test(...);
  };

  // If sizeof...(T) is zero, there shall be no member type.
  template<>
    struct common_type<>
    { };

  // If sizeof...(T) is one, the same type, if any, as common_type_t<T0, T0>.
  template<typename _Tp0>
    struct common_type<_Tp0>
    : public common_type<_Tp0, _Tp0>
    { };

  // If sizeof...(T) is two, ...
  template<typename _Tp1, typename _Tp2,
	   typename _Dp1 = __decay_t<_Tp1>, typename _Dp2 = __decay_t<_Tp2>>
    struct __common_type_impl
    {
      // If is_same_v<T1, D1> is false or is_same_v<T2, D2> is false,
      // let C denote the same type, if any, as common_type_t<D1, D2>.
      using type = common_type<_Dp1, _Dp2>;
    };

  template<typename _Tp1, typename _Tp2>
    struct __common_type_impl<_Tp1, _Tp2, _Tp1, _Tp2>
    : private __do_common_type_impl
    {
      // Otherwise, if decay_t<decltype(false ? declval<D1>() : declval<D2>())>
      // denotes a valid type, let C denote that type.
      using type = decltype(_S_test<_Tp1, _Tp2>(0));
    };

  // If sizeof...(T) is two, ...
  template<typename _Tp1, typename _Tp2>
    struct common_type<_Tp1, _Tp2>
    : public __common_type_impl<_Tp1, _Tp2>::type
    { };

  template<typename...>
    struct __common_type_pack
    { };

  template<typename, typename, typename = void>
    struct __common_type_fold;

  // If sizeof...(T) is greater than two, ...
  template<typename _Tp1, typename _Tp2, typename... _Rp>
    struct common_type<_Tp1, _Tp2, _Rp...>
    : public __common_type_fold<common_type<_Tp1, _Tp2>,
				__common_type_pack<_Rp...>>
    { };

  // Let C denote the same type, if any, as common_type_t<T1, T2>.
  // If there is such a type C, type shall denote the same type, if any,
  // as common_type_t<C, R...>.
  template<typename _CTp, typename... _Rp>
    struct __common_type_fold<_CTp, __common_type_pack<_Rp...>,
			      __void_t<typename _CTp::type>>
    : public common_type<typename _CTp::type, _Rp...>
    { };

  // Otherwise, there shall be no member type.
  template<typename _CTp, typename _Rp>
    struct __common_type_fold<_CTp, _Rp, void>
    { };

  template<typename _Tp, bool = is_enum<_Tp>::value>
    struct __underlying_type_impl
    {
      using type = __underlying_type(_Tp);
    };

  template<typename _Tp>
    struct __underlying_type_impl<_Tp, false>
    { };

  /// The underlying type of an enum.
  template<typename _Tp>
    struct underlying_type
    : public __underlying_type_impl<_Tp>
    { };

  template<typename _Tp>
    struct __declval_protector
    {
      static const bool __stop = false;
    };

  template<typename _Tp>
    auto declval() noexcept -> decltype(__declval<_Tp>(0))
    {
      static_assert(__declval_protector<_Tp>::__stop,
		    "declval() must not be used!");
      return __declval<_Tp>(0);
    }

  /// result_of
  template<typename _Signature>
    struct result_of;

  // Sfinae-friendly result_of implementation:

#define __cpp_lib_result_of_sfinae 201210

  struct __invoke_memfun_ref { };
  struct __invoke_memfun_deref { };
  struct __invoke_memobj_ref { };
  struct __invoke_memobj_deref { };
  struct __invoke_other { };

  // Associate a tag type with a specialization of __success_type.
  template<typename _Tp, typename _Tag>
    struct __result_of_success : __success_type<_Tp>
    { using __invoke_type = _Tag; };

  // [func.require] paragraph 1 bullet 1:
  struct __result_of_memfun_ref_impl
  {
    template<typename _Fp, typename _Tp1, typename... _Args>
      static __result_of_success<decltype(
      (std::declval<_Tp1>().*std::declval<_Fp>())(std::declval<_Args>()...)
      ), __invoke_memfun_ref> _S_test(int);

    template<typename...>
      static __failure_type _S_test(...);
  };

  template<typename _MemPtr, typename _Arg, typename... _Args>
    struct __result_of_memfun_ref
    : private __result_of_memfun_ref_impl
    {
      typedef decltype(_S_test<_MemPtr, _Arg, _Args...>(0)) type;
    };

  // [func.require] paragraph 1 bullet 2:
  struct __result_of_memfun_deref_impl
  {
    template<typename _Fp, typename _Tp1, typename... _Args>
      static __result_of_success<decltype(
      ((*std::declval<_Tp1>()).*std::declval<_Fp>())(std::declval<_Args>()...)
      ), __invoke_memfun_deref> _S_test(int);

    template<typename...>
      static __failure_type _S_test(...);
  };

  template<typename _MemPtr, typename _Arg, typename... _Args>
    struct __result_of_memfun_deref
    : private __result_of_memfun_deref_impl
    {
      typedef decltype(_S_test<_MemPtr, _Arg, _Args...>(0)) type;
    };

  // [func.require] paragraph 1 bullet 3:
  struct __result_of_memobj_ref_impl
  {
    template<typename _Fp, typename _Tp1>
      static __result_of_success<decltype(
      std::declval<_Tp1>().*std::declval<_Fp>()
      ), __invoke_memobj_ref> _S_test(int);

    template<typename, typename>
      static __failure_type _S_test(...);
  };

  template<typename _MemPtr, typename _Arg>
    struct __result_of_memobj_ref
    : private __result_of_memobj_ref_impl
    {
      typedef decltype(_S_test<_MemPtr, _Arg>(0)) type;
    };

  // [func.require] paragraph 1 bullet 4:
  struct __result_of_memobj_deref_impl
  {
    template<typename _Fp, typename _Tp1>
      static __result_of_success<decltype(
      (*std::declval<_Tp1>()).*std::declval<_Fp>()
      ), __invoke_memobj_deref> _S_test(int);

    template<typename, typename>
      static __failure_type _S_test(...);
  };

  template<typename _MemPtr, typename _Arg>
    struct __result_of_memobj_deref
    : private __result_of_memobj_deref_impl
    {
      typedef decltype(_S_test<_MemPtr, _Arg>(0)) type;
    };

  template<typename _MemPtr, typename _Arg>
    struct __result_of_memobj;

  template<typename _Res, typename _Class, typename _Arg>
    struct __result_of_memobj<_Res _Class::*, _Arg>
    {
      typedef __remove_cvref_t<_Arg> _Argval;
      typedef _Res _Class::* _MemPtr;
      typedef typename conditional<__or_<is_same<_Argval, _Class>,
        is_base_of<_Class, _Argval>>::value,
        __result_of_memobj_ref<_MemPtr, _Arg>,
        __result_of_memobj_deref<_MemPtr, _Arg>
      >::type::type type;
    };

  template<typename _MemPtr, typename _Arg, typename... _Args>
    struct __result_of_memfun;

  template<typename _Res, typename _Class, typename _Arg, typename... _Args>
    struct __result_of_memfun<_Res _Class::*, _Arg, _Args...>
    {
      typedef typename remove_reference<_Arg>::type _Argval;
      typedef _Res _Class::* _MemPtr;
      typedef typename conditional<is_base_of<_Class, _Argval>::value,
        __result_of_memfun_ref<_MemPtr, _Arg, _Args...>,
        __result_of_memfun_deref<_MemPtr, _Arg, _Args...>
      >::type::type type;
    };

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 2219.  INVOKE-ing a pointer to member with a reference_wrapper
  //        as the object expression

  // Used by result_of, invoke etc. to unwrap a reference_wrapper.
  template<typename _Tp, typename _Up = __remove_cvref_t<_Tp>>
    struct __inv_unwrap
    {
      using type = _Tp;
    };

  template<typename _Tp, typename _Up>
    struct __inv_unwrap<_Tp, reference_wrapper<_Up>>
    {
      using type = _Up&;
    };

  template<bool, bool, typename _Functor, typename... _ArgTypes>
    struct __result_of_impl
    {
      typedef __failure_type type;
    };

  template<typename _MemPtr, typename _Arg>
    struct __result_of_impl<true, false, _MemPtr, _Arg>
    : public __result_of_memobj<__decay_t<_MemPtr>,
				typename __inv_unwrap<_Arg>::type>
    { };

  template<typename _MemPtr, typename _Arg, typename... _Args>
    struct __result_of_impl<false, true, _MemPtr, _Arg, _Args...>
    : public __result_of_memfun<__decay_t<_MemPtr>,
				typename __inv_unwrap<_Arg>::type, _Args...>
    { };

  // [func.require] paragraph 1 bullet 5:
  struct __result_of_other_impl
  {
    template<typename _Fn, typename... _Args>
      static __result_of_success<decltype(
      std::declval<_Fn>()(std::declval<_Args>()...)
      ), __invoke_other> _S_test(int);

    template<typename...>
      static __failure_type _S_test(...);
  };

  template<typename _Functor, typename... _ArgTypes>
    struct __result_of_impl<false, false, _Functor, _ArgTypes...>
    : private __result_of_other_impl
    {
      typedef decltype(_S_test<_Functor, _ArgTypes...>(0)) type;
    };

  // __invoke_result (std::invoke_result for C++11)
  template<typename _Functor, typename... _ArgTypes>
    struct __invoke_result
    : public __result_of_impl<
        is_member_object_pointer<
          typename remove_reference<_Functor>::type
        >::value,
        is_member_function_pointer<
          typename remove_reference<_Functor>::type
        >::value,
	_Functor, _ArgTypes...
      >::type
    { };

  template<typename _Functor, typename... _ArgTypes>
    struct result_of<_Functor(_ArgTypes...)>
    : public __invoke_result<_Functor, _ArgTypes...>
    { };


  /// Alias template for aligned_storage
  template<size_t _Len, size_t _Align =
	    __alignof__(typename __aligned_storage_msa<_Len>::__type)>
    using aligned_storage_t = typename aligned_storage<_Len, _Align>::type;

  template <size_t _Len, typename... _Types>
    using aligned_union_t = typename aligned_union<_Len, _Types...>::type;

  /// Alias template for decay
  template<typename _Tp>
    using decay_t = typename decay<_Tp>::type;

  /// Alias template for enable_if
  template<bool _Cond, typename _Tp = void>
    using enable_if_t = typename enable_if<_Cond, _Tp>::type;

  /// Alias template for conditional
  template<bool _Cond, typename _Iftrue, typename _Iffalse>
    using conditional_t = typename conditional<_Cond, _Iftrue, _Iffalse>::type;

  /// Alias template for common_type
  template<typename... _Tp>
    using common_type_t = typename common_type<_Tp...>::type;

  /// Alias template for underlying_type
  template<typename _Tp>
    using underlying_type_t = typename underlying_type<_Tp>::type;

  /// Alias template for result_of
  template<typename _Tp>
    using result_of_t = typename result_of<_Tp>::type;



#define __cpp_lib_void_t 201411
  /// A metafunction that always yields void, used for detecting valid types.
  template<typename...> using void_t = void;


  /// Implementation of the detection idiom (negative case).
  template<typename _Default, typename _AlwaysVoid,
	   template<typename...> class _Op, typename... _Args>
    struct __detector
    {
      using value_t = false_type;
      using type = _Default;
    };

  /// Implementation of the detection idiom (positive case).
  template<typename _Default, template<typename...> class _Op,
	    typename... _Args>
    struct __detector<_Default, __void_t<_Op<_Args...>>, _Op, _Args...>
    {
      using value_t = true_type;
      using type = _Op<_Args...>;
    };

  // Detect whether _Op<_Args...> is a valid type, use _Default if not.
  template<typename _Default, template<typename...> class _Op,
	   typename... _Args>
    using __detected_or = __detector<_Default, void, _Op, _Args...>;

  // _Op<_Args...> if that is a valid type, otherwise _Default.
  template<typename _Default, template<typename...> class _Op,
	   typename... _Args>
    using __detected_or_t
      = typename __detected_or<_Default, _Op, _Args...>::type;

  /// @} group metaprogramming

  /**
   *  Use SFINAE to determine if the type _Tp has a publicly-accessible
   *  member type _NTYPE.
   */
#define _GLIBCXX_HAS_NESTED_TYPE(_NTYPE) template<typename _Tp, typename = __void_t<>> struct __has_ ##_NTYPE : false_type { }; template<typename _Tp> struct __has_ ##_NTYPE<_Tp, __void_t<typename _Tp::_NTYPE>> : true_type { };

  template <typename _Tp>
    struct __is_swappable;

  template <typename _Tp>
    struct __is_nothrow_swappable;

  template<typename... _Elements>
    class tuple;

  template<typename>
    struct __is_tuple_like_impl : false_type
    { };

  template<typename... _Tps>
    struct __is_tuple_like_impl<tuple<_Tps...>> : true_type
    { };

  // Internal type trait that allows us to sfinae-protect tuple_cat.
  template<typename _Tp>
    struct __is_tuple_like
    : public __is_tuple_like_impl<__remove_cvref_t<_Tp>>::type
    { };

  template<typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline
    _Require<__not_<__is_tuple_like<_Tp>>,
	     is_move_constructible<_Tp>,
	     is_move_assignable<_Tp>>
    swap(_Tp&, _Tp&)
    noexcept(__and_<is_nothrow_move_constructible<_Tp>,
	            is_nothrow_move_assignable<_Tp>>::value);

  template<typename _Tp, size_t _Nm>
    _GLIBCXX20_CONSTEXPR
    inline
    __enable_if_t<__is_swappable<_Tp>::value>
    swap(_Tp (&__a)[_Nm], _Tp (&__b)[_Nm])
    noexcept(__is_nothrow_swappable<_Tp>::value);

  namespace __swappable_details {
    using std::swap;

    struct __do_is_swappable_impl
    {
      template<typename _Tp, typename
               = decltype(swap(std::declval<_Tp&>(), std::declval<_Tp&>()))>
        static true_type __test(int);

      template<typename>
        static false_type __test(...);
    };

    struct __do_is_nothrow_swappable_impl
    {
      template<typename _Tp>
        static __bool_constant<
          noexcept(swap(std::declval<_Tp&>(), std::declval<_Tp&>()))
        > __test(int);

      template<typename>
        static false_type __test(...);
    };

  } // namespace __swappable_details

  template<typename _Tp>
    struct __is_swappable_impl
    : public __swappable_details::__do_is_swappable_impl
    {
      typedef decltype(__test<_Tp>(0)) type;
    };

  template<typename _Tp>
    struct __is_nothrow_swappable_impl
    : public __swappable_details::__do_is_nothrow_swappable_impl
    {
      typedef decltype(__test<_Tp>(0)) type;
    };

  template<typename _Tp>
    struct __is_swappable
    : public __is_swappable_impl<_Tp>::type
    { };

  template<typename _Tp>
    struct __is_nothrow_swappable
    : public __is_nothrow_swappable_impl<_Tp>::type
    { };


#define __cpp_lib_is_swappable 201603
  /// Metafunctions used for detecting swappable types: p0185r1

  /// is_swappable
  template<typename _Tp>
    struct is_swappable
    : public __is_swappable_impl<_Tp>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  /// is_nothrow_swappable
  template<typename _Tp>
    struct is_nothrow_swappable
    : public __is_nothrow_swappable_impl<_Tp>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };


  /// is_swappable_v
  template<typename _Tp>
    _GLIBCXX17_INLINE constexpr bool is_swappable_v =
      is_swappable<_Tp>::value;

  /// is_nothrow_swappable_v
  template<typename _Tp>
    _GLIBCXX17_INLINE constexpr bool is_nothrow_swappable_v =
      is_nothrow_swappable<_Tp>::value;


  namespace __swappable_with_details {
    using std::swap;

    struct __do_is_swappable_with_impl
    {
      template<typename _Tp, typename _Up, typename
               = decltype(swap(std::declval<_Tp>(), std::declval<_Up>())),
               typename
               = decltype(swap(std::declval<_Up>(), std::declval<_Tp>()))>
        static true_type __test(int);

      template<typename, typename>
        static false_type __test(...);
    };

    struct __do_is_nothrow_swappable_with_impl
    {
      template<typename _Tp, typename _Up>
        static __bool_constant<
          noexcept(swap(std::declval<_Tp>(), std::declval<_Up>()))
          &&
          noexcept(swap(std::declval<_Up>(), std::declval<_Tp>()))
        > __test(int);

      template<typename, typename>
        static false_type __test(...);
    };

  } // namespace __swappable_with_details

  template<typename _Tp, typename _Up>
    struct __is_swappable_with_impl
    : public __swappable_with_details::__do_is_swappable_with_impl
    {
      typedef decltype(__test<_Tp, _Up>(0)) type;
    };

  // Optimization for the homogenous lvalue case, not required:
  template<typename _Tp>
    struct __is_swappable_with_impl<_Tp&, _Tp&>
    : public __swappable_details::__do_is_swappable_impl
    {
      typedef decltype(__test<_Tp&>(0)) type;
    };

  template<typename _Tp, typename _Up>
    struct __is_nothrow_swappable_with_impl
    : public __swappable_with_details::__do_is_nothrow_swappable_with_impl
    {
      typedef decltype(__test<_Tp, _Up>(0)) type;
    };

  // Optimization for the homogenous lvalue case, not required:
  template<typename _Tp>
    struct __is_nothrow_swappable_with_impl<_Tp&, _Tp&>
    : public __swappable_details::__do_is_nothrow_swappable_impl
    {
      typedef decltype(__test<_Tp&>(0)) type;
    };

  /// is_swappable_with
  template<typename _Tp, typename _Up>
    struct is_swappable_with
    : public __is_swappable_with_impl<_Tp, _Up>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"first template argument must be a complete class or an unbounded array");
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Up>{}),
	"second template argument must be a complete class or an unbounded array");
    };

  /// is_nothrow_swappable_with
  template<typename _Tp, typename _Up>
    struct is_nothrow_swappable_with
    : public __is_nothrow_swappable_with_impl<_Tp, _Up>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"first template argument must be a complete class or an unbounded array");
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Up>{}),
	"second template argument must be a complete class or an unbounded array");
    };


  /// is_swappable_with_v
  template<typename _Tp, typename _Up>
    _GLIBCXX17_INLINE constexpr bool is_swappable_with_v =
      is_swappable_with<_Tp, _Up>::value;

  /// is_nothrow_swappable_with_v
  template<typename _Tp, typename _Up>
    _GLIBCXX17_INLINE constexpr bool is_nothrow_swappable_with_v =
      is_nothrow_swappable_with<_Tp, _Up>::value;




  // __is_invocable (std::is_invocable for C++11)

  // The primary template is used for invalid INVOKE expressions.
  template<typename _Result, typename _Ret,
	   bool = is_void<_Ret>::value, typename = void>
    struct __is_invocable_impl : false_type { };

  // Used for valid INVOKE and INVOKE<void> expressions.
  template<typename _Result, typename _Ret>
    struct __is_invocable_impl<_Result, _Ret,
			       /* is_void<_Ret> = */ true,
			       __void_t<typename _Result::type>>
    : true_type
    { };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wctor-dtor-privacy"
  // Used for INVOKE<R> expressions to check the implicit conversion to R.
  template<typename _Result, typename _Ret>
    struct __is_invocable_impl<_Result, _Ret,
			       /* is_void<_Ret> = */ false,
			       __void_t<typename _Result::type>>
    {
    private:
      // The type of the INVOKE expression.
      // Unlike declval, this doesn't add_rvalue_reference.
      static typename _Result::type _S_get();

      template<typename _Tp>
	static void _S_conv(_Tp);

      // This overload is viable if INVOKE(f, args...) can convert to _Tp.
      template<typename _Tp, typename = decltype(_S_conv<_Tp>(_S_get()))>
	static true_type
	_S_test(int);

      template<typename _Tp>
	static false_type
	_S_test(...);

    public:
      using type = decltype(_S_test<_Ret>(1));
    };
#pragma GCC diagnostic pop

  template<typename _Fn, typename... _ArgTypes>
    struct __is_invocable
    : __is_invocable_impl<__invoke_result<_Fn, _ArgTypes...>, void>::type
    { };

  template<typename _Fn, typename _Tp, typename... _Args>
    constexpr bool __call_is_nt(__invoke_memfun_ref)
    {
      using _Up = typename __inv_unwrap<_Tp>::type;
      return noexcept((std::declval<_Up>().*std::declval<_Fn>())(
	    std::declval<_Args>()...));
    }

  template<typename _Fn, typename _Tp, typename... _Args>
    constexpr bool __call_is_nt(__invoke_memfun_deref)
    {
      return noexcept(((*std::declval<_Tp>()).*std::declval<_Fn>())(
	    std::declval<_Args>()...));
    }

  template<typename _Fn, typename _Tp>
    constexpr bool __call_is_nt(__invoke_memobj_ref)
    {
      using _Up = typename __inv_unwrap<_Tp>::type;
      return noexcept(std::declval<_Up>().*std::declval<_Fn>());
    }

  template<typename _Fn, typename _Tp>
    constexpr bool __call_is_nt(__invoke_memobj_deref)
    {
      return noexcept((*std::declval<_Tp>()).*std::declval<_Fn>());
    }

  template<typename _Fn, typename... _Args>
    constexpr bool __call_is_nt(__invoke_other)
    {
      return noexcept(std::declval<_Fn>()(std::declval<_Args>()...));
    }

  template<typename _Result, typename _Fn, typename... _Args>
    struct __call_is_nothrow
    : __bool_constant<
	std::__call_is_nt<_Fn, _Args...>(typename _Result::__invoke_type{})
      >
    { };

  template<typename _Fn, typename... _Args>
    using __call_is_nothrow_
      = __call_is_nothrow<__invoke_result<_Fn, _Args...>, _Fn, _Args...>;

  // __is_nothrow_invocable (std::is_nothrow_invocable for C++11)
  template<typename _Fn, typename... _Args>
    struct __is_nothrow_invocable
    : __and_<__is_invocable<_Fn, _Args...>,
             __call_is_nothrow_<_Fn, _Args...>>::type
    { };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wctor-dtor-privacy"
  struct __nonesuchbase {};
  struct __nonesuch : private __nonesuchbase {
    ~__nonesuch() = delete;
    __nonesuch(__nonesuch const&) = delete;
    void operator=(__nonesuch const&) = delete;
  };
#pragma GCC diagnostic pop


#define __cpp_lib_is_invocable 201703

  /// std::invoke_result
  template<typename _Functor, typename... _ArgTypes>
    struct invoke_result
    : public __invoke_result<_Functor, _ArgTypes...>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Functor>{}),
	"_Functor must be a complete class or an unbounded array");
      static_assert((std::__is_complete_or_unbounded(
	__type_identity<_ArgTypes>{}) && ...),
	"each argument type must be a complete class or an unbounded array");
    };

  /// std::invoke_result_t
  template<typename _Fn, typename... _Args>
    using invoke_result_t = typename invoke_result<_Fn, _Args...>::type;

  /// std::is_invocable
  template<typename _Fn, typename... _ArgTypes>
    struct is_invocable
    : __is_invocable_impl<__invoke_result<_Fn, _ArgTypes...>, void>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Fn>{}),
	"_Fn must be a complete class or an unbounded array");
      static_assert((std::__is_complete_or_unbounded(
	__type_identity<_ArgTypes>{}) && ...),
	"each argument type must be a complete class or an unbounded array");
    };

  /// std::is_invocable_r
  template<typename _Ret, typename _Fn, typename... _ArgTypes>
    struct is_invocable_r
    : __is_invocable_impl<__invoke_result<_Fn, _ArgTypes...>, _Ret>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Fn>{}),
	"_Fn must be a complete class or an unbounded array");
      static_assert((std::__is_complete_or_unbounded(
	__type_identity<_ArgTypes>{}) && ...),
	"each argument type must be a complete class or an unbounded array");
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Ret>{}),
	"_Ret must be a complete class or an unbounded array");
    };

  /// std::is_nothrow_invocable
  template<typename _Fn, typename... _ArgTypes>
    struct is_nothrow_invocable
    : __and_<__is_invocable_impl<__invoke_result<_Fn, _ArgTypes...>, void>,
	     __call_is_nothrow_<_Fn, _ArgTypes...>>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Fn>{}),
	"_Fn must be a complete class or an unbounded array");
      static_assert((std::__is_complete_or_unbounded(
	__type_identity<_ArgTypes>{}) && ...),
	"each argument type must be a complete class or an unbounded array");
    };

  template<typename _Result, typename _Ret, typename = void>
    struct __is_nt_invocable_impl : false_type { };

  template<typename _Result, typename _Ret>
    struct __is_nt_invocable_impl<_Result, _Ret,
				  __void_t<typename _Result::type>>
    : __or_<is_void<_Ret>,
	    __is_nothrow_convertible<typename _Result::type, _Ret>>
    { };

  /// std::is_nothrow_invocable_r
  template<typename _Ret, typename _Fn, typename... _ArgTypes>
    struct is_nothrow_invocable_r
    : __and_<__is_nt_invocable_impl<__invoke_result<_Fn, _ArgTypes...>, _Ret>,
             __call_is_nothrow_<_Fn, _ArgTypes...>>::type
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Fn>{}),
	"_Fn must be a complete class or an unbounded array");
      static_assert((std::__is_complete_or_unbounded(
	__type_identity<_ArgTypes>{}) && ...),
	"each argument type must be a complete class or an unbounded array");
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Ret>{}),
	"_Ret must be a complete class or an unbounded array");
    };

  /// std::is_invocable_v
  template<typename _Fn, typename... _Args>
    inline constexpr bool is_invocable_v = is_invocable<_Fn, _Args...>::value;

  /// std::is_nothrow_invocable_v
  template<typename _Fn, typename... _Args>
    inline constexpr bool is_nothrow_invocable_v
      = is_nothrow_invocable<_Fn, _Args...>::value;

  /// std::is_invocable_r_v
  template<typename _Ret, typename _Fn, typename... _Args>
    inline constexpr bool is_invocable_r_v
      = is_invocable_r<_Ret, _Fn, _Args...>::value;

  /// std::is_nothrow_invocable_r_v
  template<typename _Ret, typename _Fn, typename... _Args>
    inline constexpr bool is_nothrow_invocable_r_v
      = is_nothrow_invocable_r<_Ret, _Fn, _Args...>::value;



#define __cpp_lib_type_trait_variable_templates 201510L
template <typename _Tp>
  inline constexpr bool is_void_v = is_void<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_null_pointer_v = is_null_pointer<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_integral_v = is_integral<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_floating_point_v = is_floating_point<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_array_v = is_array<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_pointer_v = is_pointer<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_lvalue_reference_v =
    is_lvalue_reference<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_rvalue_reference_v =
    is_rvalue_reference<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_member_object_pointer_v =
    is_member_object_pointer<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_member_function_pointer_v =
    is_member_function_pointer<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_enum_v = is_enum<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_union_v = is_union<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_class_v = is_class<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_function_v = is_function<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_reference_v = is_reference<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_arithmetic_v = is_arithmetic<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_fundamental_v = is_fundamental<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_object_v = is_object<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_scalar_v = is_scalar<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_compound_v = is_compound<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_member_pointer_v = is_member_pointer<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_const_v = is_const<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_volatile_v = is_volatile<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_trivial_v = is_trivial<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_trivially_copyable_v =
    is_trivially_copyable<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_standard_layout_v = is_standard_layout<_Tp>::value;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
template <typename _Tp>
  _GLIBCXX20_DEPRECATED("use is_standard_layout_v && is_trivial_v instead")
  inline constexpr bool is_pod_v = is_pod<_Tp>::value;
template <typename _Tp>
  _GLIBCXX17_DEPRECATED
  inline constexpr bool is_literal_type_v = is_literal_type<_Tp>::value;
#pragma GCC diagnostic pop
 template <typename _Tp>
  inline constexpr bool is_empty_v = is_empty<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_polymorphic_v = is_polymorphic<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_abstract_v = is_abstract<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_final_v = is_final<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_signed_v = is_signed<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_unsigned_v = is_unsigned<_Tp>::value;
template <typename _Tp, typename... _Args>
  inline constexpr bool is_constructible_v =
    is_constructible<_Tp, _Args...>::value;
template <typename _Tp>
  inline constexpr bool is_default_constructible_v =
    is_default_constructible<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_copy_constructible_v =
    is_copy_constructible<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_move_constructible_v =
    is_move_constructible<_Tp>::value;
template <typename _Tp, typename _Up>
  inline constexpr bool is_assignable_v = is_assignable<_Tp, _Up>::value;
template <typename _Tp>
  inline constexpr bool is_copy_assignable_v = is_copy_assignable<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_move_assignable_v = is_move_assignable<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_destructible_v = is_destructible<_Tp>::value;
template <typename _Tp, typename... _Args>
  inline constexpr bool is_trivially_constructible_v =
    is_trivially_constructible<_Tp, _Args...>::value;
template <typename _Tp>
  inline constexpr bool is_trivially_default_constructible_v =
    is_trivially_default_constructible<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_trivially_copy_constructible_v =
    is_trivially_copy_constructible<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_trivially_move_constructible_v =
    is_trivially_move_constructible<_Tp>::value;
template <typename _Tp, typename _Up>
  inline constexpr bool is_trivially_assignable_v =
    is_trivially_assignable<_Tp, _Up>::value;
template <typename _Tp>
  inline constexpr bool is_trivially_copy_assignable_v =
    is_trivially_copy_assignable<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_trivially_move_assignable_v =
    is_trivially_move_assignable<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_trivially_destructible_v =
    is_trivially_destructible<_Tp>::value;
template <typename _Tp, typename... _Args>
  inline constexpr bool is_nothrow_constructible_v =
    is_nothrow_constructible<_Tp, _Args...>::value;
template <typename _Tp>
  inline constexpr bool is_nothrow_default_constructible_v =
    is_nothrow_default_constructible<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_nothrow_copy_constructible_v =
    is_nothrow_copy_constructible<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_nothrow_move_constructible_v =
    is_nothrow_move_constructible<_Tp>::value;
template <typename _Tp, typename _Up>
  inline constexpr bool is_nothrow_assignable_v =
    is_nothrow_assignable<_Tp, _Up>::value;
template <typename _Tp>
  inline constexpr bool is_nothrow_copy_assignable_v =
    is_nothrow_copy_assignable<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_nothrow_move_assignable_v =
    is_nothrow_move_assignable<_Tp>::value;
template <typename _Tp>
  inline constexpr bool is_nothrow_destructible_v =
    is_nothrow_destructible<_Tp>::value;
template <typename _Tp>
  inline constexpr bool has_virtual_destructor_v =
    has_virtual_destructor<_Tp>::value;
template <typename _Tp>
  inline constexpr size_t alignment_of_v = alignment_of<_Tp>::value;
template <typename _Tp>
  inline constexpr size_t rank_v = rank<_Tp>::value;
template <typename _Tp, unsigned _Idx = 0>
  inline constexpr size_t extent_v = extent<_Tp, _Idx>::value;

template <typename _Tp, typename _Up>
  inline constexpr bool is_same_v = __is_same(_Tp, _Up);




template <typename _Base, typename _Derived>
  inline constexpr bool is_base_of_v = is_base_of<_Base, _Derived>::value;
template <typename _From, typename _To>
  inline constexpr bool is_convertible_v = is_convertible<_From, _To>::value;


#define __cpp_lib_has_unique_object_representations 201606
  /// has_unique_object_representations
  template<typename _Tp>
    struct has_unique_object_representations
    : bool_constant<__has_unique_object_representations(
      remove_cv_t<remove_all_extents_t<_Tp>>
      )>
    {
      static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{}),
	"template argument must be a complete class or an unbounded array");
    };

  template<typename _Tp>
    inline constexpr bool has_unique_object_representations_v
      = has_unique_object_representations<_Tp>::value;



#define __cpp_lib_is_aggregate 201703
  /// is_aggregate
  template<typename _Tp>
    struct is_aggregate
    : bool_constant<__is_aggregate(remove_cv_t<_Tp>)>
    { };

  /// is_aggregate_v
  template<typename _Tp>
    inline constexpr bool is_aggregate_v = is_aggregate<_Tp>::value;




_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std




namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @addtogroup utilities
   *  @{
   */

  /**
   *  @brief  Forward an lvalue.
   *  @return The parameter cast to the specified type.
   *
   *  This function is used to implement "perfect forwarding".
   */
  template<typename _Tp>
    constexpr _Tp&&
    forward(typename std::remove_reference<_Tp>::type& __t) noexcept
    { return static_cast<_Tp&&>(__t); }

  /**
   *  @brief  Forward an rvalue.
   *  @return The parameter cast to the specified type.
   *
   *  This function is used to implement "perfect forwarding".
   */
  template<typename _Tp>
    constexpr _Tp&&
    forward(typename std::remove_reference<_Tp>::type&& __t) noexcept
    {
      static_assert(!std::is_lvalue_reference<_Tp>::value, "template argument"
		    " substituting _Tp is an lvalue reference type");
      return static_cast<_Tp&&>(__t);
    }

  /**
   *  @brief  Convert a value to an rvalue.
   *  @param  __t  A thing of arbitrary type.
   *  @return The parameter cast to an rvalue-reference to allow moving it.
  */
  template<typename _Tp>
    constexpr typename std::remove_reference<_Tp>::type&&
    move(_Tp&& __t) noexcept
    { return static_cast<typename std::remove_reference<_Tp>::type&&>(__t); }


  template<typename _Tp>
    struct __move_if_noexcept_cond
    : public __and_<__not_<is_nothrow_move_constructible<_Tp>>,
                    is_copy_constructible<_Tp>>::type { };

  /**
   *  @brief  Conditionally convert a value to an rvalue.
   *  @param  __x  A thing of arbitrary type.
   *  @return The parameter, possibly cast to an rvalue-reference.
   *
   *  Same as std::move unless the type's move constructor could throw and the
   *  type is copyable, in which case an lvalue-reference is returned instead.
   */
  template<typename _Tp>
    constexpr typename
    conditional<__move_if_noexcept_cond<_Tp>::value, const _Tp&, _Tp&&>::type
    move_if_noexcept(_Tp& __x) noexcept
    { return std::move(__x); }

  // declval, from type_traits.


  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 2296. std::addressof should be constexpr
#define __cpp_lib_addressof_constexpr 201603

  /**
   *  @brief Returns the actual address of the object or function
   *         referenced by r, even in the presence of an overloaded
   *         operator&.
   *  @param  __r  Reference to an object or function.
   *  @return   The actual address.
  */
  template<typename _Tp>
    inline _GLIBCXX17_CONSTEXPR _Tp*
    addressof(_Tp& __r) noexcept
    { return std::__addressof(__r); }

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 2598. addressof works on temporaries
  template<typename _Tp>
    const _Tp* addressof(const _Tp&&) = delete;

  // C++11 version of std::exchange for internal use.
  template <typename _Tp, typename _Up = _Tp>
    _GLIBCXX20_CONSTEXPR
    inline _Tp
    __exchange(_Tp& __obj, _Up&& __new_val)
    {
      _Tp __old_val = std::move(__obj);
      __obj = std::forward<_Up>(__new_val);
      return __old_val;
    }

  /// @} group utilities

#define _GLIBCXX_FWDREF(_Tp) _Tp&&
#define _GLIBCXX_MOVE(__val) std::move(__val)
#define _GLIBCXX_FORWARD(_Tp,__val) std::forward<_Tp>(__val)






  /**
   *  @addtogroup utilities
   *  @{
   */

  /**
   *  @brief Swaps two values.
   *  @param  __a  A thing of arbitrary type.
   *  @param  __b  Another thing of arbitrary type.
   *  @return   Nothing.
  */
  template<typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline

    typename enable_if<__and_<__not_<__is_tuple_like<_Tp>>,
			      is_move_constructible<_Tp>,
			      is_move_assignable<_Tp>>::value>::type



    swap(_Tp& __a, _Tp& __b)
    _GLIBCXX_NOEXCEPT_IF(__and_<is_nothrow_move_constructible<_Tp>,
				is_nothrow_move_assignable<_Tp>>::value)
    {




      _Tp __tmp = _GLIBCXX_MOVE(__a);
      __a = _GLIBCXX_MOVE(__b);
      __b = _GLIBCXX_MOVE(__tmp);
    }

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // DR 809. std::swap should be overloaded for array types.
  /// Swap the contents of two arrays.
  template<typename _Tp, size_t _Nm>
    _GLIBCXX20_CONSTEXPR
    inline

    typename enable_if<__is_swappable<_Tp>::value>::type



    swap(_Tp (&__a)[_Nm], _Tp (&__b)[_Nm])
    _GLIBCXX_NOEXCEPT_IF(__is_nothrow_swappable<_Tp>::value)
    {
      for (size_t __n = 0; __n < _Nm; ++__n)
	swap(__a[__n], __b[__n]);
    }

  /// @} group utilities
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace


extern "C++" {

namespace std
{
  /**
   * @addtogroup exceptions
   * @{
   */

  /// Exception class with exception_ptr data member.
  class nested_exception
  {
    exception_ptr _M_ptr;

  public:
    nested_exception() noexcept : _M_ptr(current_exception()) { }

    nested_exception(const nested_exception&) noexcept = default;

    nested_exception& operator=(const nested_exception&) noexcept = default;

    virtual ~nested_exception() noexcept;

    [[noreturn]]
    void
    rethrow_nested() const
    {
      if (_M_ptr)
	rethrow_exception(_M_ptr);
      std::terminate();
    }

    exception_ptr
    nested_ptr() const noexcept
    { return _M_ptr; }
  };

  /// @cond undocumented

  template<typename _Except>
    struct _Nested_exception : public _Except, public nested_exception
    {
      explicit _Nested_exception(const _Except& __ex)
      : _Except(__ex)
      { }

      explicit _Nested_exception(_Except&& __ex)
      : _Except(static_cast<_Except&&>(__ex))
      { }
    };

  // [except.nested]/8
  // Throw an exception of unspecified type that is publicly derived from
  // both remove_reference_t<_Tp> and nested_exception.
  template<typename _Tp>
    [[noreturn]]
    inline void
    __throw_with_nested_impl(_Tp&& __t, true_type)
    {
      using _Up = typename remove_reference<_Tp>::type;
      throw _Nested_exception<_Up>{std::forward<_Tp>(__t)};
    }

  template<typename _Tp>
    [[noreturn]]
    inline void
    __throw_with_nested_impl(_Tp&& __t, false_type)
    { throw std::forward<_Tp>(__t); }

  /// @endcond

  /// If @p __t is derived from nested_exception, throws @p __t.
  /// Else, throws an implementation-defined object derived from both.
  template<typename _Tp>
    [[noreturn]]
    inline void
    throw_with_nested(_Tp&& __t)
    {
      using _Up = typename decay<_Tp>::type;
      using _CopyConstructible
	= __and_<is_copy_constructible<_Up>, is_move_constructible<_Up>>;
      static_assert(_CopyConstructible::value,
	  "throw_with_nested argument must be CopyConstructible");
      using __nest = __and_<is_class<_Up>, __bool_constant<!__is_final(_Up)>,
			    __not_<is_base_of<nested_exception, _Up>>>;
      std::__throw_with_nested_impl(std::forward<_Tp>(__t), __nest{});
    }

  /// @cond undocumented

  // Determine if dynamic_cast<const nested_exception&> would be well-formed.
  template<typename _Tp>
    using __rethrow_if_nested_cond = typename enable_if<
      __and_<is_polymorphic<_Tp>,
	     __or_<__not_<is_base_of<nested_exception, _Tp>>,
		   is_convertible<_Tp*, nested_exception*>>>::value
    >::type;

  // Attempt dynamic_cast to nested_exception and call rethrow_nested().
  template<typename _Ex>
    inline __rethrow_if_nested_cond<_Ex>
    __rethrow_if_nested_impl(const _Ex* __ptr)
    {
      if (auto __ne_ptr = dynamic_cast<const nested_exception*>(__ptr))
	__ne_ptr->rethrow_nested();
    }

  // Otherwise, no effects.
  inline void
  __rethrow_if_nested_impl(const void*)
  { }

  /// @endcond

  /// If @p __ex is derived from nested_exception, @p __ex.rethrow_nested().
  template<typename _Ex>
    inline void
    rethrow_if_nested(const _Ex& __ex)
    { std::__rethrow_if_nested_impl(std::__addressof(__ex)); }

  // @} group exceptions
} // namespace std

} // extern "C++"



#pragma GCC visibility pop



// Character Traits for use by standard string and iostream -*- C++ -*-

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/char_traits.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{string}
 */

//
// ISO C++ 14882: 21  Strings library
//


#define _CHAR_TRAITS_H 1

       

// Core algorithmic facilities -*- C++ -*-

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996-1998
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/** @file bits/stl_algobase.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{algorithm}
 */


#define _STL_ALGOBASE_H 1


// Function-Based Exception Support -*- C++ -*-

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/functexcept.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{exception}
 *
 *  This header provides support for -fno-exceptions.
 */

//
// ISO C++ 14882: 19.1  Exception classes
//


#define _FUNCTEXCEPT_H 1




namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Helper for exception objects in <except>
  void
  __throw_bad_exception(void) __attribute__((__noreturn__));

  // Helper for exception objects in <new>
  void
  __throw_bad_alloc(void) __attribute__((__noreturn__));

  void
  __throw_bad_array_new_length(void) __attribute__((__noreturn__));

  // Helper for exception objects in <typeinfo>
  void
  __throw_bad_cast(void) __attribute__((__noreturn__));

  void
  __throw_bad_typeid(void) __attribute__((__noreturn__));

  // Helpers for exception objects in <stdexcept>
  void
  __throw_logic_error(const char*) __attribute__((__noreturn__));

  void
  __throw_domain_error(const char*) __attribute__((__noreturn__));

  void
  __throw_invalid_argument(const char*) __attribute__((__noreturn__));

  void
  __throw_length_error(const char*) __attribute__((__noreturn__));

  void
  __throw_out_of_range(const char*) __attribute__((__noreturn__));

  void
  __throw_out_of_range_fmt(const char*, ...) __attribute__((__noreturn__))
    __attribute__((__format__(__gnu_printf__, 1, 2)));

  void
  __throw_runtime_error(const char*) __attribute__((__noreturn__));

  void
  __throw_range_error(const char*) __attribute__((__noreturn__));

  void
  __throw_overflow_error(const char*) __attribute__((__noreturn__));

  void
  __throw_underflow_error(const char*) __attribute__((__noreturn__));

  // Helpers for exception objects in <ios>
  void
  __throw_ios_failure(const char*) __attribute__((__noreturn__));

  void
  __throw_ios_failure(const char*, int) __attribute__((__noreturn__));

  // Helpers for exception objects in <system_error>
  void
  __throw_system_error(int) __attribute__((__noreturn__));

  // Helpers for exception objects in <future>
  void
  __throw_future_error(int) __attribute__((__noreturn__));

  // Helpers for exception objects in <functional>
  void
  __throw_bad_function_call() __attribute__((__noreturn__));

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// The  -*- C++ -*- type traits classes for internal use in libstdc++

// Copyright (C) 2000-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/cpp_type_traits.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{ext/type_traits}
 */

// Written by Gabriel Dos Reis <dosreis@cmla.ens-cachan.fr>


#define _CPP_TYPE_TRAITS_H 1

       



//
// This file provides some compile-time information about various types.
// These representations were designed, on purpose, to be constant-expressions
// and not types as found in <bits/type_traits.h>.  In particular, they
// can be used in control structures and the optimizer hopefully will do
// the obvious thing.
//
// Why integral expressions, and not functions nor types?
// Firstly, these compile-time entities are used as template-arguments
// so function return values won't work:  We need compile-time entities.
// We're left with types and constant  integral expressions.
// Secondly, from the point of view of ease of use, type-based compile-time
// information is -not- *that* convenient.  One has to write lots of
// overloaded functions and to hope that the compiler will select the right
// one. As a net effect, the overall structure isn't very clear at first
// glance.
// Thirdly, partial ordering and overload resolution (of function templates)
// is highly costly in terms of compiler-resource.  It is a Good Thing to
// keep these resource consumption as least as possible.
//
// See valarray_array.h for a case use.
//
// -- Gaby (dosreis@cmla.ens-cachan.fr) 2000-03-06.
//
// Update 2005: types are also provided and <bits/type_traits.h> has been
// removed.
//

extern "C++" {

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  struct __true_type { };
  struct __false_type { };

  template<bool>
    struct __truth_type
    { typedef __false_type __type; };

  template<>
    struct __truth_type<true>
    { typedef __true_type __type; };

  // N.B. The conversions to bool are needed due to the issue
  // explained in c++/19404.
  template<class _Sp, class _Tp>
    struct __traitor
    {
      enum { __value = bool(_Sp::__value) || bool(_Tp::__value) };
      typedef typename __truth_type<__value>::__type __type;
    };

  // Compare for equality of types.
  template<typename, typename>
    struct __are_same
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };

  template<typename _Tp>
    struct __are_same<_Tp, _Tp>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  // Holds if the template-argument is a void type.
  template<typename _Tp>
    struct __is_void
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };

  template<>
    struct __is_void<void>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  //
  // Integer types
  //
  template<typename _Tp>
    struct __is_integer
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };

  // Thirteen specializations (yes there are eleven standard integer
  // types; <em>long long</em> and <em>unsigned long long</em> are
  // supported as extensions).  Up to four target-specific __int<N>
  // types are supported as well.
  template<>
    struct __is_integer<bool>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_integer<char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_integer<signed char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_integer<unsigned char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };


  template<>
    struct __is_integer<wchar_t>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };




  template<>
    struct __is_integer<char16_t>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_integer<char32_t>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };


  template<>
    struct __is_integer<short>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_integer<unsigned short>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_integer<int>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_integer<unsigned int>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_integer<long>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_integer<unsigned long>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_integer<long long>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_integer<unsigned long long>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

#define __INT_N(TYPE) template<> struct __is_integer<TYPE> { enum { __value = 1 }; typedef __true_type __type; }; template<> struct __is_integer<unsigned TYPE> { enum { __value = 1 }; typedef __true_type __type; };














#undef __INT_N

  //
  // Floating point types
  //
  template<typename _Tp>
    struct __is_floating
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };

  // three specializations (float, double and 'long double')
  template<>
    struct __is_floating<float>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_floating<double>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_floating<long double>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  //
  // Pointer types
  //
  template<typename _Tp>
    struct __is_pointer
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };

  template<typename _Tp>
    struct __is_pointer<_Tp*>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  //
  // An arithmetic type is an integer type or a floating point type
  //
  template<typename _Tp>
    struct __is_arithmetic
    : public __traitor<__is_integer<_Tp>, __is_floating<_Tp> >
    { };

  //
  // A scalar type is an arithmetic type or a pointer type
  // 
  template<typename _Tp>
    struct __is_scalar
    : public __traitor<__is_arithmetic<_Tp>, __is_pointer<_Tp> >
    { };

  //
  // For use in std::copy and std::find overloads for streambuf iterators.
  //
  template<typename _Tp>
    struct __is_char
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };

  template<>
    struct __is_char<char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };


  template<>
    struct __is_char<wchar_t>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };


  template<typename _Tp>
    struct __is_byte
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };

  template<>
    struct __is_byte<char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_byte<signed char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<>
    struct __is_byte<unsigned char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };


  enum class byte : unsigned char;

  template<>
    struct __is_byte<byte>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };



  template<typename> struct iterator_traits;

  // A type that is safe for use with memcpy, memmove, memcmp etc.
  template<typename _Tp>
    struct __is_nonvolatile_trivially_copyable
    {
      enum { __value = __is_trivially_copyable(_Tp) };
    };

  // Cannot use memcpy/memmove/memcmp on volatile types even if they are
  // trivially copyable, so ensure __memcpyable<volatile int*, volatile int*>
  // and similar will be false.
  template<typename _Tp>
    struct __is_nonvolatile_trivially_copyable<volatile _Tp>
    {
      enum { __value = 0 };
    };

  // Whether two iterator types can be used with memcpy/memmove.
  template<typename _OutputIter, typename _InputIter>
    struct __memcpyable
    {
      enum { __value = 0 };
    };

  template<typename _Tp>
    struct __memcpyable<_Tp*, _Tp*>
    : __is_nonvolatile_trivially_copyable<_Tp>
    { };

  template<typename _Tp>
    struct __memcpyable<_Tp*, const _Tp*>
    : __is_nonvolatile_trivially_copyable<_Tp>
    { };

  // Whether two iterator types can be used with memcmp.
  // This trait only says it's well-formed to use memcmp, not that it
  // gives the right answer for a given algorithm. So for example, std::equal
  // needs to add additional checks that the types are integers or pointers,
  // because other trivially copyable types can overload operator==.
  template<typename _Iter1, typename _Iter2>
    struct __memcmpable
    {
      enum { __value = 0 };
    };

  // OK to use memcmp with pointers to trivially copyable types.
  template<typename _Tp>
    struct __memcmpable<_Tp*, _Tp*>
    : __is_nonvolatile_trivially_copyable<_Tp>
    { };

  template<typename _Tp>
    struct __memcmpable<const _Tp*, _Tp*>
    : __is_nonvolatile_trivially_copyable<_Tp>
    { };

  template<typename _Tp>
    struct __memcmpable<_Tp*, const _Tp*>
    : __is_nonvolatile_trivially_copyable<_Tp>
    { };

  // Whether memcmp can be used to determine ordering for a type
  // e.g. in std::lexicographical_compare or three-way comparisons.
  // True for unsigned integer-like types where comparing each byte in turn
  // as an unsigned char yields the right result. This is true for all
  // unsigned integers on big endian targets, but only unsigned narrow
  // character types (and std::byte) on little endian targets.
  template<typename _Tp, bool _TreatAsBytes =



	__is_byte<_Tp>::__value

    >
    struct __is_memcmp_ordered
    {
      static const bool __value = _Tp(-1) > _Tp(1); // is unsigned
    };

  template<typename _Tp>
    struct __is_memcmp_ordered<_Tp, false>
    {
      static const bool __value = false;
    };

  // Whether two types can be compared using memcmp.
  template<typename _Tp, typename _Up, bool = sizeof(_Tp) == sizeof(_Up)>
    struct __is_memcmp_ordered_with
    {
      static const bool __value = __is_memcmp_ordered<_Tp>::__value
	&& __is_memcmp_ordered<_Up>::__value;
    };

  template<typename _Tp, typename _Up>
    struct __is_memcmp_ordered_with<_Tp, _Up, false>
    {
      static const bool __value = false;
    };









  // std::byte can only be compared to itself, not to other types.
  template<>
    struct __is_memcmp_ordered_with<std::byte, std::byte, true>
    { static constexpr bool __value = true; };

  template<typename _Tp, bool _SameSize>
    struct __is_memcmp_ordered_with<_Tp, std::byte, _SameSize>
    { static constexpr bool __value = false; };

  template<typename _Up, bool _SameSize>
    struct __is_memcmp_ordered_with<std::byte, _Up, _SameSize>
    { static constexpr bool __value = false; };


  //
  // Move iterator type
  //
  template<typename _Tp>
    struct __is_move_iterator
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };

  // Fallback implementation of the function in bits/stl_iterator.h used to
  // remove the move_iterator wrapper.
  template<typename _Iterator>
    _GLIBCXX20_CONSTEXPR
    inline _Iterator
    __miter_base(_Iterator __it)
    { return __it; }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
} // extern "C++"

// -*- C++ -*-

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file ext/type_traits.h
 *  This file is a GNU extension to the Standard C++ Library.
 */


#define _EXT_TYPE_TRAITS 1

       




extern "C++" {

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Define a nested type if some predicate holds.
  template<bool, typename>
    struct __enable_if 
    { };

  template<typename _Tp>
    struct __enable_if<true, _Tp>
    { typedef _Tp __type; };


  // Conditional expression for types. If true, first, if false, second.
  template<bool _Cond, typename _Iftrue, typename _Iffalse>
    struct __conditional_type
    { typedef _Iftrue __type; };

  template<typename _Iftrue, typename _Iffalse>
    struct __conditional_type<false, _Iftrue, _Iffalse>
    { typedef _Iffalse __type; };


  // Given an integral builtin type, return the corresponding unsigned type.
  template<typename _Tp>
    struct __add_unsigned
    { 
    private:
      typedef __enable_if<std::__is_integer<_Tp>::__value, _Tp> __if_type;
      
    public:
      typedef typename __if_type::__type __type; 
    };

  template<>
    struct __add_unsigned<char>
    { typedef unsigned char __type; };

  template<>
    struct __add_unsigned<signed char>
    { typedef unsigned char __type; };

  template<>
    struct __add_unsigned<short>
    { typedef unsigned short __type; };

  template<>
    struct __add_unsigned<int>
    { typedef unsigned int __type; };

  template<>
    struct __add_unsigned<long>
    { typedef unsigned long __type; };

  template<>
    struct __add_unsigned<long long>
    { typedef unsigned long long __type; };

  // Declare but don't define.
  template<>
    struct __add_unsigned<bool>;

  template<>
    struct __add_unsigned<wchar_t>;


  // Given an integral builtin type, return the corresponding signed type.
  template<typename _Tp>
    struct __remove_unsigned
    { 
    private:
      typedef __enable_if<std::__is_integer<_Tp>::__value, _Tp> __if_type;
      
    public:
      typedef typename __if_type::__type __type; 
    };

  template<>
    struct __remove_unsigned<char>
    { typedef signed char __type; };

  template<>
    struct __remove_unsigned<unsigned char>
    { typedef signed char __type; };

  template<>
    struct __remove_unsigned<unsigned short>
    { typedef short __type; };

  template<>
    struct __remove_unsigned<unsigned int>
    { typedef int __type; };

  template<>
    struct __remove_unsigned<unsigned long>
    { typedef long __type; };

  template<>
    struct __remove_unsigned<unsigned long long>
    { typedef long long __type; };

  // Declare but don't define.
  template<>
    struct __remove_unsigned<bool>;

  template<>
    struct __remove_unsigned<wchar_t>;


  // For use in string and vstring.
  template<typename _Type>
    inline bool
    __is_null_pointer(_Type* __ptr)
    { return __ptr == 0; }

  template<typename _Type>
    inline bool
    __is_null_pointer(_Type)
    { return false; }


  inline bool
  __is_null_pointer(std::nullptr_t)
  { return true; }


  // For complex and cmath
  template<typename _Tp, bool = std::__is_integer<_Tp>::__value>
    struct __promote
    { typedef double __type; };

  // No nested __type member for non-integer non-floating point types,
  // allows this type to be used for SFINAE to constrain overloads in
  // <cmath> and <complex> to only the intended types.
  template<typename _Tp>
    struct __promote<_Tp, false>
    { };

  template<>
    struct __promote<long double>
    { typedef long double __type; };

  template<>
    struct __promote<double>
    { typedef double __type; };

  template<>
    struct __promote<float>
    { typedef float __type; };

  template<typename _Tp, typename _Up,
           typename _Tp2 = typename __promote<_Tp>::__type,
           typename _Up2 = typename __promote<_Up>::__type>
    struct __promote_2
    {
      typedef __typeof__(_Tp2() + _Up2()) __type;
    };

  template<typename _Tp, typename _Up, typename _Vp,
           typename _Tp2 = typename __promote<_Tp>::__type,
           typename _Up2 = typename __promote<_Up>::__type,
           typename _Vp2 = typename __promote<_Vp>::__type>
    struct __promote_3
    {
      typedef __typeof__(_Tp2() + _Up2() + _Vp2()) __type;
    };

  template<typename _Tp, typename _Up, typename _Vp, typename _Wp,
           typename _Tp2 = typename __promote<_Tp>::__type,
           typename _Up2 = typename __promote<_Up>::__type,
           typename _Vp2 = typename __promote<_Vp>::__type,
           typename _Wp2 = typename __promote<_Wp>::__type>
    struct __promote_4
    {
      typedef __typeof__(_Tp2() + _Up2() + _Vp2() + _Wp2()) __type;
    };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
} // extern "C++"

// -*- C++ -*-

// Copyright (C) 2007-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file ext/numeric_traits.h
 *  This file is a GNU extension to the Standard C++ Library.
 */


#define _EXT_NUMERIC_TRAITS 1

       




namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Compile time constants for builtin types.
  // In C++98 std::numeric_limits member functions are not constant expressions
  // (that changed in C++11 with the addition of 'constexpr').
  // Even for C++11, this header is smaller than <limits> and can be used
  // when only is_signed, digits, min, or max values are needed for integers,
  // or is_signed, digits10, max_digits10, or max_exponent10 for floats.

  // Unlike __is_integer (and std::is_integral) this trait is true for
  // non-standard built-in integer types such as __int128 and __int20.
  template<typename _Tp>
    struct __is_integer_nonstrict
    : public std::__is_integer<_Tp>
    {
      using std::__is_integer<_Tp>::__value;

      // The number of bits in the value representation.
      enum { __width = __value ? sizeof(_Tp) * __CHAR_BIT__ : 0 };
    };

  template<typename _Value>
    struct __numeric_traits_integer
    {

      static_assert(__is_integer_nonstrict<_Value>::__value,
		    "invalid specialization");


      // NB: these two are also available in std::numeric_limits as compile
      // time constants, but <limits> is big and we can avoid including it.
      static const bool __is_signed = (_Value)(-1) < 0;
      static const int __digits
	= __is_integer_nonstrict<_Value>::__width - __is_signed;

      // The initializers must be constants so that __max and __min are too.
      static const _Value __max = __is_signed
	? (((((_Value)1 << (__digits - 1)) - 1) << 1) + 1)
	: ~(_Value)0;
      static const _Value __min = __is_signed ? -__max - 1 : (_Value)0;
    };

  template<typename _Value>
    const _Value __numeric_traits_integer<_Value>::__min;

  template<typename _Value>
    const _Value __numeric_traits_integer<_Value>::__max;

  template<typename _Value>
    const bool __numeric_traits_integer<_Value>::__is_signed;

  template<typename _Value>
    const int __numeric_traits_integer<_Value>::__digits;

  // Enable __numeric_traits_integer for types where the __is_integer_nonstrict
  // primary template doesn't give the right answer.
#define _GLIBCXX_INT_N_TRAITS(T,WIDTH) template<> struct __is_integer_nonstrict<T> { enum { __value = 1 }; typedef std::__true_type __type; enum { __width = WIDTH }; }; template<> struct __is_integer_nonstrict<unsigned T> { enum { __value = 1 }; typedef std::__true_type __type; enum { __width = WIDTH }; };

  // We need to specify the width for some __intNN types because they
  // have padding bits, e.g. the object representation of __int20 has 32 bits,
  // but its width (number of bits in the value representation) is only 20.














  // In strict modes __is_integer<__int128> is false,
  // but we still want to define __numeric_traits_integer<__int128>.
  _GLIBCXX_INT_N_TRAITS(__int128, 128)


#undef _GLIBCXX_INT_N_TRAITS


  /// Convenience alias for __numeric_traits<integer-type>.
  template<typename _Tp>
    using __int_traits = __numeric_traits_integer<_Tp>;


#define __glibcxx_floating(_Tp,_Fval,_Dval,_LDval) (std::__are_same<_Tp, float>::__value ? _Fval : std::__are_same<_Tp, double>::__value ? _Dval : _LDval)



#define __glibcxx_max_digits10(_Tp) (2 + __glibcxx_floating(_Tp, __FLT_MANT_DIG__, __DBL_MANT_DIG__, __LDBL_MANT_DIG__) * 643L / 2136)



#define __glibcxx_digits10(_Tp) __glibcxx_floating(_Tp, __FLT_DIG__, __DBL_DIG__, __LDBL_DIG__)


#define __glibcxx_max_exponent10(_Tp) __glibcxx_floating(_Tp, __FLT_MAX_10_EXP__, __DBL_MAX_10_EXP__, __LDBL_MAX_10_EXP__)



  // N.B. this only supports float, double and long double (no __float128 etc.)
  template<typename _Value>
    struct __numeric_traits_floating
    {
      // Only floating point types. See N1822.
      static const int __max_digits10 = __glibcxx_max_digits10(_Value);

      // See above comment...
      static const bool __is_signed = true;
      static const int __digits10 = __glibcxx_digits10(_Value);
      static const int __max_exponent10 = __glibcxx_max_exponent10(_Value);
    };

  template<typename _Value>
    const int __numeric_traits_floating<_Value>::__max_digits10;

  template<typename _Value>
    const bool __numeric_traits_floating<_Value>::__is_signed;

  template<typename _Value>
    const int __numeric_traits_floating<_Value>::__digits10;

  template<typename _Value>
    const int __numeric_traits_floating<_Value>::__max_exponent10;

#undef __glibcxx_floating
#undef __glibcxx_max_digits10
#undef __glibcxx_digits10
#undef __glibcxx_max_exponent10

  template<typename _Value>
    struct __numeric_traits
    : public __numeric_traits_integer<_Value>
    { };

  template<>
    struct __numeric_traits<float>
    : public __numeric_traits_floating<float>
    { };

  template<>
    struct __numeric_traits<double>
    : public __numeric_traits_floating<double>
    { };

  template<>
    struct __numeric_traits<long double>
    : public __numeric_traits_floating<long double>
    { };


_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// Pair implementation -*- C++ -*-

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996,1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/** @file bits/stl_pair.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{utility}
 */


#define _STL_PAIR_H 1











namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @addtogroup utilities
   *  @{
   */


  /// Tag type for piecewise construction of std::pair objects.
  struct piecewise_construct_t { explicit piecewise_construct_t() = default; };

  /// Tag for piecewise construction of std::pair objects.
  _GLIBCXX17_INLINE constexpr piecewise_construct_t piecewise_construct =
    piecewise_construct_t();

  /// @cond undocumented

  // Forward declarations.
  template<typename...>
    class tuple;

  template<size_t...>
    struct _Index_tuple;

  // Concept utility functions, reused in conditionally-explicit
  // constructors.
  // See PR 70437, don't look at is_constructible or
  // is_convertible if the types are the same to
  // avoid querying those properties for incomplete types.
  template <bool, typename _T1, typename _T2>
    struct _PCC
    {
      template <typename _U1, typename _U2>
      static constexpr bool _ConstructiblePair()
      {
	return __and_<is_constructible<_T1, const _U1&>,
		      is_constructible<_T2, const _U2&>>::value;
      }

      template <typename _U1, typename _U2>
      static constexpr bool _ImplicitlyConvertiblePair()
      {
	return __and_<is_convertible<const _U1&, _T1>,
		      is_convertible<const _U2&, _T2>>::value;
      }

      template <typename _U1, typename _U2>
      static constexpr bool _MoveConstructiblePair()
      {
	return __and_<is_constructible<_T1, _U1&&>,
		      is_constructible<_T2, _U2&&>>::value;
      }

      template <typename _U1, typename _U2>
      static constexpr bool _ImplicitlyMoveConvertiblePair()
      {
	return __and_<is_convertible<_U1&&, _T1>,
		      is_convertible<_U2&&, _T2>>::value;
      }

      template <bool __implicit, typename _U1, typename _U2>
      static constexpr bool _CopyMovePair()
      {
	using __do_converts = __and_<is_convertible<const _U1&, _T1>,
				  is_convertible<_U2&&, _T2>>;
	using __converts = typename conditional<__implicit,
				       __do_converts,
				       __not_<__do_converts>>::type;
	return __and_<is_constructible<_T1, const _U1&>,
		      is_constructible<_T2, _U2&&>,
		      __converts
		      >::value;
      }

      template <bool __implicit, typename _U1, typename _U2>
      static constexpr bool _MoveCopyPair()
      {
	using __do_converts = __and_<is_convertible<_U1&&, _T1>,
				  is_convertible<const _U2&, _T2>>;
	using __converts = typename conditional<__implicit,
				       __do_converts,
				       __not_<__do_converts>>::type;
	return __and_<is_constructible<_T1, _U1&&>,
		      is_constructible<_T2, const _U2&&>,
		      __converts
		      >::value;
      }
  };

  template <typename _T1, typename _T2>
    struct _PCC<false, _T1, _T2>
    {
      template <typename _U1, typename _U2>
      static constexpr bool _ConstructiblePair()
      {
	return false;
      }

      template <typename _U1, typename _U2>
      static constexpr bool _ImplicitlyConvertiblePair()
      {
	return false;
      }

      template <typename _U1, typename _U2>
      static constexpr bool _MoveConstructiblePair()
      {
	return false;
      }

      template <typename _U1, typename _U2>
      static constexpr bool _ImplicitlyMoveConvertiblePair()
      {
	return false;
      }
  };


  template<typename _U1, typename _U2> class __pair_base
  {

    template<typename _T1, typename _T2> friend struct pair;
    __pair_base() = default;
    ~__pair_base() = default;
    __pair_base(const __pair_base&) = default;
    __pair_base& operator=(const __pair_base&) = delete;

  };

  /// @endcond

 /**
   *  @brief Struct holding two objects of arbitrary type.
   *
   *  @tparam _T1  Type of first object.
   *  @tparam _T2  Type of second object.
   *
   *  <https://gcc.gnu.org/onlinedocs/libstdc++/manual/utilities.html>
   */
  template<typename _T1, typename _T2>
    struct pair
    : private __pair_base<_T1, _T2>
    {
      typedef _T1 first_type;    ///< The type of the `first` member
      typedef _T2 second_type;   ///< The type of the `second` member

      _T1 first;                 ///< The first member
      _T2 second;                ///< The second member

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 265.  std::pair::pair() effects overly restrictive
      /** The default constructor creates @c first and @c second using their
       *  respective default constructors.  */

      template <typename _U1 = _T1,
                typename _U2 = _T2,
                typename enable_if<__and_<
                                     __is_implicitly_default_constructible<_U1>,
                                     __is_implicitly_default_constructible<_U2>>
                                   ::value, bool>::type = true>

      _GLIBCXX_CONSTEXPR pair()
      : first(), second() { }


      template <typename _U1 = _T1,
                typename _U2 = _T2,
                typename enable_if<__and_<
                       is_default_constructible<_U1>,
                       is_default_constructible<_U2>,
                       __not_<
                         __and_<__is_implicitly_default_constructible<_U1>,
                                __is_implicitly_default_constructible<_U2>>>>
                                   ::value, bool>::type = false>
      explicit constexpr pair()
      : first(), second() { }







      // Shortcut for constraining the templates that don't take pairs.
      /// @cond undocumented
      using _PCCP = _PCC<true, _T1, _T2>;
      /// @endcond

      /// Construct from two const lvalues, allowing implicit conversions.
      template<typename _U1 = _T1, typename _U2=_T2, typename
	       enable_if<_PCCP::template
			   _ConstructiblePair<_U1, _U2>()
	                 && _PCCP::template
			   _ImplicitlyConvertiblePair<_U1, _U2>(),
                         bool>::type=true>
      constexpr pair(const _T1& __a, const _T2& __b)
      : first(__a), second(__b) { }

      /// Construct from two const lvalues, disallowing implicit conversions.
       template<typename _U1 = _T1, typename _U2=_T2, typename
		enable_if<_PCCP::template
			    _ConstructiblePair<_U1, _U2>()
	                  && !_PCCP::template
			    _ImplicitlyConvertiblePair<_U1, _U2>(),
                         bool>::type=false>
      explicit constexpr pair(const _T1& __a, const _T2& __b)
      : first(__a), second(__b) { }








      // Shortcut for constraining the templates that take pairs.
      /// @cond undocumented
      template <typename _U1, typename _U2>
        using _PCCFP = _PCC<!is_same<_T1, _U1>::value
			    || !is_same<_T2, _U2>::value,
			    _T1, _T2>;
      /// @endcond

      template<typename _U1, typename _U2, typename
	       enable_if<_PCCFP<_U1, _U2>::template
			   _ConstructiblePair<_U1, _U2>()
	                 && _PCCFP<_U1, _U2>::template
			   _ImplicitlyConvertiblePair<_U1, _U2>(),
			  bool>::type=true>
        constexpr pair(const pair<_U1, _U2>& __p)
        : first(__p.first), second(__p.second) { }

      template<typename _U1, typename _U2, typename
	       enable_if<_PCCFP<_U1, _U2>::template
			   _ConstructiblePair<_U1, _U2>()
			 && !_PCCFP<_U1, _U2>::template
			   _ImplicitlyConvertiblePair<_U1, _U2>(),
                         bool>::type=false>
	explicit constexpr pair(const pair<_U1, _U2>& __p)
	: first(__p.first), second(__p.second) { }



      constexpr pair(const pair&) = default;	///< Copy constructor
      constexpr pair(pair&&) = default;		///< Move constructor

      // DR 811.
      template<typename _U1, typename
	       enable_if<_PCCP::template
			   _MoveCopyPair<true, _U1, _T2>(),
                         bool>::type=true>
       constexpr pair(_U1&& __x, const _T2& __y)
       : first(std::forward<_U1>(__x)), second(__y) { }

      template<typename _U1, typename
	       enable_if<_PCCP::template
			   _MoveCopyPair<false, _U1, _T2>(),
                         bool>::type=false>
       explicit constexpr pair(_U1&& __x, const _T2& __y)
       : first(std::forward<_U1>(__x)), second(__y) { }

      template<typename _U2, typename
	       enable_if<_PCCP::template
			   _CopyMovePair<true, _T1, _U2>(),
                         bool>::type=true>
       constexpr pair(const _T1& __x, _U2&& __y)
       : first(__x), second(std::forward<_U2>(__y)) { }

      template<typename _U2, typename
	       enable_if<_PCCP::template
			   _CopyMovePair<false, _T1, _U2>(),
                         bool>::type=false>
       explicit pair(const _T1& __x, _U2&& __y)
       : first(__x), second(std::forward<_U2>(__y)) { }

      template<typename _U1, typename _U2, typename
	       enable_if<_PCCP::template
			   _MoveConstructiblePair<_U1, _U2>()
			  && _PCCP::template
			   _ImplicitlyMoveConvertiblePair<_U1, _U2>(),
                         bool>::type=true>
	constexpr pair(_U1&& __x, _U2&& __y)
	: first(std::forward<_U1>(__x)), second(std::forward<_U2>(__y)) { }

      template<typename _U1, typename _U2, typename
	       enable_if<_PCCP::template
			   _MoveConstructiblePair<_U1, _U2>()
			  && !_PCCP::template
			   _ImplicitlyMoveConvertiblePair<_U1, _U2>(),
                         bool>::type=false>
	explicit constexpr pair(_U1&& __x, _U2&& __y)
	: first(std::forward<_U1>(__x)), second(std::forward<_U2>(__y)) { }


      template<typename _U1, typename _U2, typename
	       enable_if<_PCCFP<_U1, _U2>::template
			   _MoveConstructiblePair<_U1, _U2>()
			  && _PCCFP<_U1, _U2>::template
			   _ImplicitlyMoveConvertiblePair<_U1, _U2>(),
                         bool>::type=true>
	constexpr pair(pair<_U1, _U2>&& __p)
	: first(std::forward<_U1>(__p.first)),
	  second(std::forward<_U2>(__p.second)) { }

      template<typename _U1, typename _U2, typename
	       enable_if<_PCCFP<_U1, _U2>::template
			   _MoveConstructiblePair<_U1, _U2>()
			  && !_PCCFP<_U1, _U2>::template
			   _ImplicitlyMoveConvertiblePair<_U1, _U2>(),
                         bool>::type=false>
	explicit constexpr pair(pair<_U1, _U2>&& __p)
	: first(std::forward<_U1>(__p.first)),
	  second(std::forward<_U2>(__p.second)) { }

      template<typename... _Args1, typename... _Args2>
	_GLIBCXX20_CONSTEXPR
        pair(piecewise_construct_t, tuple<_Args1...>, tuple<_Args2...>);

      _GLIBCXX20_CONSTEXPR pair&
      operator=(typename conditional<
		__and_<is_copy_assignable<_T1>,
		       is_copy_assignable<_T2>>::value,
		const pair&, const __nonesuch&>::type __p)
      {
	first = __p.first;
	second = __p.second;
	return *this;
      }

      _GLIBCXX20_CONSTEXPR pair&
      operator=(typename conditional<
		__and_<is_move_assignable<_T1>,
		       is_move_assignable<_T2>>::value,
		pair&&, __nonesuch&&>::type __p)
      noexcept(__and_<is_nothrow_move_assignable<_T1>,
		      is_nothrow_move_assignable<_T2>>::value)
      {
	first = std::forward<first_type>(__p.first);
	second = std::forward<second_type>(__p.second);
	return *this;
      }

      template<typename _U1, typename _U2>
	_GLIBCXX20_CONSTEXPR
	typename enable_if<__and_<is_assignable<_T1&, const _U1&>,
				  is_assignable<_T2&, const _U2&>>::value,
			   pair&>::type
	operator=(const pair<_U1, _U2>& __p)
	{
	  first = __p.first;
	  second = __p.second;
	  return *this;
	}

      template<typename _U1, typename _U2>
	_GLIBCXX20_CONSTEXPR
	typename enable_if<__and_<is_assignable<_T1&, _U1&&>,
				  is_assignable<_T2&, _U2&&>>::value,
			   pair&>::type
	operator=(pair<_U1, _U2>&& __p)
	{
	  first = std::forward<_U1>(__p.first);
	  second = std::forward<_U2>(__p.second);
	  return *this;
	}

      /// Swap the first members and then the second members.
      _GLIBCXX20_CONSTEXPR void
      swap(pair& __p)
      noexcept(__and_<__is_nothrow_swappable<_T1>,
                      __is_nothrow_swappable<_T2>>::value)
      {
	using std::swap;
	swap(first, __p.first);
	swap(second, __p.second);
      }

    private:
      template<typename... _Args1, size_t... _Indexes1,
	       typename... _Args2, size_t... _Indexes2>
	_GLIBCXX20_CONSTEXPR
        pair(tuple<_Args1...>&, tuple<_Args2...>&,
	     _Index_tuple<_Indexes1...>, _Index_tuple<_Indexes2...>);

    };

  /// @relates pair @{


  template<typename _T1, typename _T2> pair(_T1, _T2) -> pair<_T1, _T2>;


  /// Two pairs of the same type are equal iff their members are equal.
  template<typename _T1, typename _T2>
    inline _GLIBCXX_CONSTEXPR bool
    operator==(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return __x.first == __y.first && __x.second == __y.second; }

  /** Defines a lexicographical order for pairs.
   *
   * For two pairs of the same type, `P` is ordered before `Q` if
   * `P.first` is less than `Q.first`, or if `P.first` and `Q.first`
   * are equivalent (neither is less than the other) and `P.second` is less
   * than `Q.second`.
  */
  template<typename _T1, typename _T2>
    inline _GLIBCXX_CONSTEXPR bool
    operator<(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return __x.first < __y.first
	     || (!(__y.first < __x.first) && __x.second < __y.second); }

  /// Uses @c operator== to find the result.
  template<typename _T1, typename _T2>
    inline _GLIBCXX_CONSTEXPR bool
    operator!=(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return !(__x == __y); }

  /// Uses @c operator< to find the result.
  template<typename _T1, typename _T2>
    inline _GLIBCXX_CONSTEXPR bool
    operator>(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return __y < __x; }

  /// Uses @c operator< to find the result.
  template<typename _T1, typename _T2>
    inline _GLIBCXX_CONSTEXPR bool
    operator<=(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return !(__y < __x); }

  /// Uses @c operator< to find the result.
  template<typename _T1, typename _T2>
    inline _GLIBCXX_CONSTEXPR bool
    operator>=(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return !(__x < __y); }



  /** Swap overload for pairs. Calls std::pair::swap().
   *
   * @note This std::swap overload is not declared in C++03 mode,
   * which has performance implications, e.g. see https://gcc.gnu.org/PR38466
  */
  template<typename _T1, typename _T2>
    _GLIBCXX20_CONSTEXPR inline

    // Constrained free swap overload, see p0185r1
    typename enable_if<__and_<__is_swappable<_T1>,
                              __is_swappable<_T2>>::value>::type



    swap(pair<_T1, _T2>& __x, pair<_T1, _T2>& __y)
    noexcept(noexcept(__x.swap(__y)))
    { __x.swap(__y); }


  template<typename _T1, typename _T2>
    typename enable_if<!__and_<__is_swappable<_T1>,
			       __is_swappable<_T2>>::value>::type
    swap(pair<_T1, _T2>&, pair<_T1, _T2>&) = delete;



  // @} relates pair

  /**
   *  @brief A convenience wrapper for creating a pair from two objects.
   *  @param  __x  The first object.
   *  @param  __y  The second object.
   *  @return   A newly-constructed pair<> object of the appropriate type.
   *
   *  The C++98 standard says the objects are passed by reference-to-const,
   *  but C++03 says they are passed by value (this was LWG issue #181).
   *
   *  Since C++11 they have been passed by forwarding reference and then
   *  forwarded to the new members of the pair. To create a pair with a
   *  member of reference type, pass a `reference_wrapper` to this function.
   */
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 181.  make_pair() unintended behavior

  // NB: DR 706.
  template<typename _T1, typename _T2>
    constexpr pair<typename __decay_and_strip<_T1>::__type,
                   typename __decay_and_strip<_T2>::__type>
    make_pair(_T1&& __x, _T2&& __y)
    {
      typedef typename __decay_and_strip<_T1>::__type __ds_type1;
      typedef typename __decay_and_strip<_T2>::__type __ds_type2;
      typedef pair<__ds_type1, __ds_type2> 	      __pair_type;
      return __pair_type(std::forward<_T1>(__x), std::forward<_T2>(__y));
    }







  /// @}

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

// Types used in iterator implementation -*- C++ -*-

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996-1998
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/** @file bits/stl_iterator_base_types.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iterator}
 *
 *  This file contains all of the general iterator-related utility types,
 *  such as iterator_traits and struct iterator.
 */


#define _STL_ITERATOR_BASE_TYPES_H 1

       











namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @defgroup iterators Iterators
   *  Abstractions for uniform iterating through various underlying types.
  */
  //@{ 

  /**
   *  @defgroup iterator_tags Iterator Tags
   *  These are empty types, used to distinguish different iterators.  The
   *  distinction is not made by what they contain, but simply by what they
   *  are.  Different underlying algorithms can then be used based on the
   *  different operations supported by different iterator types.
  */
  //@{ 
  ///  Marking input iterators.
  struct input_iterator_tag { };

  ///  Marking output iterators.
  struct output_iterator_tag { };

  /// Forward iterators support a superset of input iterator operations.
  struct forward_iterator_tag : public input_iterator_tag { };

  /// Bidirectional iterators support a superset of forward iterator
  /// operations.
  struct bidirectional_iterator_tag : public forward_iterator_tag { };

  /// Random-access iterators support a superset of bidirectional
  /// iterator operations.
  struct random_access_iterator_tag : public bidirectional_iterator_tag { };





  //@}

  /**
   *  @brief  Common %iterator class.
   *
   *  This class does nothing but define nested typedefs.  %Iterator classes
   *  can inherit from this class to save some work.  The typedefs are then
   *  used in specializations and overloading.
   *
   *  In particular, there are no default implementations of requirements
   *  such as @c operator++ and the like.  (How could there be?)
  */
  template<typename _Category, typename _Tp, typename _Distance = ptrdiff_t,
           typename _Pointer = _Tp*, typename _Reference = _Tp&>
    struct iterator
    {
      /// One of the @link iterator_tags tag types@endlink.
      typedef _Category  iterator_category;
      /// The type "pointed to" by the iterator.
      typedef _Tp        value_type;
      /// Distance between iterators is represented as this type.
      typedef _Distance  difference_type;
      /// This type represents a pointer-to-value_type.
      typedef _Pointer   pointer;
      /// This type represents a reference-to-value_type.
      typedef _Reference reference;
    };

  /**
   *  @brief  Traits class for iterators.
   *
   *  This class does nothing but define nested typedefs.  The general
   *  version simply @a forwards the nested typedefs from the Iterator
   *  argument.  Specialized versions for pointers and pointers-to-const
   *  provide tighter, more correct semantics.
  */
  template<typename _Iterator>
    struct iterator_traits;


  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 2408. SFINAE-friendly common_type/iterator_traits is missing in C++14
  template<typename _Iterator, typename = __void_t<>>
    struct __iterator_traits { };



  template<typename _Iterator>
    struct __iterator_traits<_Iterator,
			     __void_t<typename _Iterator::iterator_category,
				      typename _Iterator::value_type,
				      typename _Iterator::difference_type,
				      typename _Iterator::pointer,
				      typename _Iterator::reference>>
    {
      typedef typename _Iterator::iterator_category iterator_category;
      typedef typename _Iterator::value_type        value_type;
      typedef typename _Iterator::difference_type   difference_type;
      typedef typename _Iterator::pointer           pointer;
      typedef typename _Iterator::reference         reference;
    };


  template<typename _Iterator>
    struct iterator_traits
    : public __iterator_traits<_Iterator> { };


  /// Partial specialization for pointer types.
  template<typename _Tp>
    struct iterator_traits<_Tp*>
    {
      typedef random_access_iterator_tag iterator_category;
      typedef _Tp                         value_type;
      typedef ptrdiff_t                   difference_type;
      typedef _Tp*                        pointer;
      typedef _Tp&                        reference;
    };

  /// Partial specialization for const pointer types.
  template<typename _Tp>
    struct iterator_traits<const _Tp*>
    {
      typedef random_access_iterator_tag iterator_category;
      typedef _Tp                         value_type;
      typedef ptrdiff_t                   difference_type;
      typedef const _Tp*                  pointer;
      typedef const _Tp&                  reference;
    };


  /**
   *  This function is not a part of the C++ standard but is syntactic
   *  sugar for internal library use only.
  */
  template<typename _Iter>
    inline _GLIBCXX_CONSTEXPR
    typename iterator_traits<_Iter>::iterator_category
    __iterator_category(const _Iter&)
    { return typename iterator_traits<_Iter>::iterator_category(); }

  //@}


  template<typename _Iter>
    using __iterator_category_t
      = typename iterator_traits<_Iter>::iterator_category;

  template<typename _InIter>
    using _RequireInputIter =
      __enable_if_t<is_convertible<__iterator_category_t<_InIter>,
				   input_iterator_tag>::value>;

  template<typename _It,
	   typename _Cat = __iterator_category_t<_It>>
    struct __is_random_access_iter
      : is_base_of<random_access_iterator_tag, _Cat>
    {
      typedef is_base_of<random_access_iterator_tag, _Cat> _Base;
      enum { __value = _Base::value };
    };







_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// Functions used by iterators -*- C++ -*-

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996-1998
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/** @file bits/stl_iterator_base_funcs.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iterator}
 *
 *  This file contains all of the general iterator-related utility
 *  functions, such as distance() and advance().
 */


#define _STL_ITERATOR_BASE_FUNCS_H 1

       

// Concept-checking control -*- C++ -*-

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/concept_check.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iterator}
 */


#define _CONCEPT_CHECK_H 1

       



// All places in libstdc++-v3 where these are used, or /might/ be used, or
// don't need to be used, or perhaps /should/ be used, are commented with
// "concept requirements" (and maybe some more text).  So grep like crazy
// if you're looking for additional places to use these.

// Concept-checking code is off by default unless users turn it on via
// configure options or editing c++config.h.
// It is not supported for freestanding implementations.



#define __glibcxx_function_requires(...) 
#define __glibcxx_class_requires(_a,_b) 
#define __glibcxx_class_requires2(_a,_b,_c) 
#define __glibcxx_class_requires3(_a,_b,_c,_d) 
#define __glibcxx_class_requires4(_a,_b,_c,_d,_e) 


// Debugging support implementation -*- C++ -*-

// Copyright (C) 2003-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file debug/assertions.h
 *  This file is a GNU debug extension to the Standard C++ Library.
 */


#define _GLIBCXX_DEBUG_ASSERTIONS_H 1



#define _GLIBCXX_DEBUG_ASSERT(_Condition) 
#define _GLIBCXX_DEBUG_PEDASSERT(_Condition) 
#define _GLIBCXX_DEBUG_ONLY(_Statement) 




#define __glibcxx_requires_non_empty_range(_First,_Last) 
#define __glibcxx_requires_nonempty() 
#define __glibcxx_requires_subscript(_N) 



namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

_GLIBCXX_BEGIN_NAMESPACE_CONTAINER
  // Forward declaration for the overloads of __distance.
  template <typename> struct _List_iterator;
  template <typename> struct _List_const_iterator;
_GLIBCXX_END_NAMESPACE_CONTAINER

  template<typename _InputIterator>
    inline _GLIBCXX14_CONSTEXPR
    typename iterator_traits<_InputIterator>::difference_type
    __distance(_InputIterator __first, _InputIterator __last,
               input_iterator_tag)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)

      typename iterator_traits<_InputIterator>::difference_type __n = 0;
      while (__first != __last)
	{
	  ++__first;
	  ++__n;
	}
      return __n;
    }

  template<typename _RandomAccessIterator>
    inline _GLIBCXX14_CONSTEXPR
    typename iterator_traits<_RandomAccessIterator>::difference_type
    __distance(_RandomAccessIterator __first, _RandomAccessIterator __last,
               random_access_iterator_tag)
    {
      // concept requirements
      __glibcxx_function_requires(_RandomAccessIteratorConcept<
				  _RandomAccessIterator>)
      return __last - __first;
    }


  // Forward declaration because of the qualified call in distance.
  template<typename _Tp>
    ptrdiff_t
    __distance(_GLIBCXX_STD_C::_List_iterator<_Tp>,
	       _GLIBCXX_STD_C::_List_iterator<_Tp>,
	       input_iterator_tag);

  template<typename _Tp>
    ptrdiff_t
    __distance(_GLIBCXX_STD_C::_List_const_iterator<_Tp>,
	       _GLIBCXX_STD_C::_List_const_iterator<_Tp>,
	       input_iterator_tag);


  /**
   *  @brief A generalization of pointer arithmetic.
   *  @param  __first  An input iterator.
   *  @param  __last  An input iterator.
   *  @return  The distance between them.
   *
   *  Returns @c n such that __first + n == __last.  This requires
   *  that @p __last must be reachable from @p __first.  Note that @c
   *  n may be negative.
   *
   *  For random access iterators, this uses their @c + and @c - operations
   *  and are constant time.  For other %iterator classes they are linear time.
  */
  template<typename _InputIterator>
    inline _GLIBCXX17_CONSTEXPR
    typename iterator_traits<_InputIterator>::difference_type
    distance(_InputIterator __first, _InputIterator __last)
    {
      // concept requirements -- taken care of in __distance
      return std::__distance(__first, __last,
			     std::__iterator_category(__first));
    }

  template<typename _InputIterator, typename _Distance>
    inline _GLIBCXX14_CONSTEXPR void
    __advance(_InputIterator& __i, _Distance __n, input_iterator_tag)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
      __glibcxx_assert(__n >= 0);
      while (__n--)
	++__i;
    }

  template<typename _BidirectionalIterator, typename _Distance>
    inline _GLIBCXX14_CONSTEXPR void
    __advance(_BidirectionalIterator& __i, _Distance __n,
	      bidirectional_iterator_tag)
    {
      // concept requirements
      __glibcxx_function_requires(_BidirectionalIteratorConcept<
				  _BidirectionalIterator>)
      if (__n > 0)
        while (__n--)
	  ++__i;
      else
        while (__n++)
	  --__i;
    }

  template<typename _RandomAccessIterator, typename _Distance>
    inline _GLIBCXX14_CONSTEXPR void
    __advance(_RandomAccessIterator& __i, _Distance __n,
              random_access_iterator_tag)
    {
      // concept requirements
      __glibcxx_function_requires(_RandomAccessIteratorConcept<
				  _RandomAccessIterator>)
      if (__builtin_constant_p(__n) && __n == 1)
	++__i;
      else if (__builtin_constant_p(__n) && __n == -1)
	--__i;
      else
	__i += __n;
    }

  /**
   *  @brief A generalization of pointer arithmetic.
   *  @param  __i  An input iterator.
   *  @param  __n  The @a delta by which to change @p __i.
   *  @return  Nothing.
   *
   *  This increments @p i by @p n.  For bidirectional and random access
   *  iterators, @p __n may be negative, in which case @p __i is decremented.
   *
   *  For random access iterators, this uses their @c + and @c - operations
   *  and are constant time.  For other %iterator classes they are linear time.
  */
  template<typename _InputIterator, typename _Distance>
    inline _GLIBCXX17_CONSTEXPR void
    advance(_InputIterator& __i, _Distance __n)
    {
      // concept requirements -- taken care of in __advance
      typename iterator_traits<_InputIterator>::difference_type __d = __n;
      std::__advance(__i, __d, std::__iterator_category(__i));
    }



  template<typename _InputIterator>
    inline _GLIBCXX17_CONSTEXPR _InputIterator
    next(_InputIterator __x, typename
	 iterator_traits<_InputIterator>::difference_type __n = 1)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
      std::advance(__x, __n);
      return __x;
    }

  template<typename _BidirectionalIterator>
    inline _GLIBCXX17_CONSTEXPR _BidirectionalIterator
    prev(_BidirectionalIterator __x, typename
	 iterator_traits<_BidirectionalIterator>::difference_type __n = 1) 
    {
      // concept requirements
      __glibcxx_function_requires(_BidirectionalIteratorConcept<
				  _BidirectionalIterator>)
      std::advance(__x, -__n);
      return __x;
    }



_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// Iterators -*- C++ -*-

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996-1998
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/** @file bits/stl_iterator.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iterator}
 *
 *  This file implements reverse_iterator, back_insert_iterator,
 *  front_insert_iterator, insert_iterator, __normal_iterator, and their
 *  supporting functions and overloaded operators.
 */


#define _STL_ITERATOR_H 1




// Pointer Traits -*- C++ -*-

// Copyright (C) 2011-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/ptr_traits.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory}
 */


#define _PTR_TRAITS_H 1










namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  class __undefined;

  // Given Template<T, ...> return T, otherwise invalid.
  template<typename _Tp>
    struct __get_first_arg
    { using type = __undefined; };

  template<template<typename, typename...> class _Template, typename _Tp,
           typename... _Types>
    struct __get_first_arg<_Template<_Tp, _Types...>>
    { using type = _Tp; };

  template<typename _Tp>
    using __get_first_arg_t = typename __get_first_arg<_Tp>::type;

  // Given Template<T, ...> and U return Template<U, ...>, otherwise invalid.
  template<typename _Tp, typename _Up>
    struct __replace_first_arg
    { };

  template<template<typename, typename...> class _Template, typename _Up,
           typename _Tp, typename... _Types>
    struct __replace_first_arg<_Template<_Tp, _Types...>, _Up>
    { using type = _Template<_Up, _Types...>; };

  template<typename _Tp, typename _Up>
    using __replace_first_arg_t = typename __replace_first_arg<_Tp, _Up>::type;

  template<typename _Tp>
    using __make_not_void
      = typename conditional<is_void<_Tp>::value, __undefined, _Tp>::type;

  /**
   * @brief  Uniform interface to all pointer-like types
   * @ingroup pointer_abstractions
  */
  template<typename _Ptr>
    struct pointer_traits
    {
    private:
      template<typename _Tp>
	using __element_type = typename _Tp::element_type;

      template<typename _Tp>
	using __difference_type = typename _Tp::difference_type;

      template<typename _Tp, typename _Up, typename = void>
	struct __rebind : __replace_first_arg<_Tp, _Up> { };

      template<typename _Tp, typename _Up>
	struct __rebind<_Tp, _Up, __void_t<typename _Tp::template rebind<_Up>>>
	{ using type = typename _Tp::template rebind<_Up>; };

    public:
      /// The pointer type.
      using pointer = _Ptr;

      /// The type pointed to.
      using element_type
	= __detected_or_t<__get_first_arg_t<_Ptr>, __element_type, _Ptr>;

      /// The type used to represent the difference between two pointers.
      using difference_type
	= __detected_or_t<ptrdiff_t, __difference_type, _Ptr>;

      /// A pointer to a different type.
      template<typename _Up>
        using rebind = typename __rebind<_Ptr, _Up>::type;

      static _Ptr
      pointer_to(__make_not_void<element_type>& __e)
      { return _Ptr::pointer_to(__e); }

      static_assert(!is_same<element_type, __undefined>::value,
	  "pointer type defines element_type or is like SomePointer<T, Args>");
    };

  /**
   * @brief  Partial specialization for built-in pointers.
   * @ingroup pointer_abstractions
  */
  template<typename _Tp>
    struct pointer_traits<_Tp*>
    {
      /// The pointer type
      typedef _Tp* pointer;
      /// The type pointed to
      typedef _Tp  element_type;
      /// Type used to represent the difference between two pointers
      typedef ptrdiff_t difference_type;

      template<typename _Up>
        using rebind = _Up*;

      /**
       *  @brief  Obtain a pointer to an object
       *  @param  __r  A reference to an object of type @c element_type
       *  @return @c addressof(__r)
      */
      static _GLIBCXX20_CONSTEXPR pointer
      pointer_to(__make_not_void<element_type>& __r) noexcept
      { return std::addressof(__r); }
    };

  /// Convenience alias for rebinding pointers.
  template<typename _Ptr, typename _Tp>
    using __ptr_rebind = typename pointer_traits<_Ptr>::template rebind<_Tp>;

  template<typename _Tp>
    constexpr _Tp*
    __to_address(_Tp* __ptr) noexcept
    {
      static_assert(!std::is_function<_Tp>::value, "not a function pointer");
      return __ptr;
    }


  template<typename _Ptr>
    constexpr typename std::pointer_traits<_Ptr>::element_type*
    __to_address(const _Ptr& __ptr)
    { return std::__to_address(__ptr.operator->()); }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std












#define __cpp_lib_array_constexpr 201803L









namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   * @addtogroup iterators
   * @{
   */


  // 24.4.1 Reverse iterators
  /**
   *  Bidirectional and random access iterators have corresponding reverse
   *  %iterator adaptors that iterate through the data structure in the
   *  opposite direction.  They have the same signatures as the corresponding
   *  iterators.  The fundamental relation between a reverse %iterator and its
   *  corresponding %iterator @c i is established by the identity:
   *  @code
   *      &*(reverse_iterator(i)) == &*(i - 1)
   *  @endcode
   *
   *  <em>This mapping is dictated by the fact that while there is always a
   *  pointer past the end of an array, there might not be a valid pointer
   *  before the beginning of an array.</em> [24.4.1]/1,2
   *
   *  Reverse iterators can be tricky and surprising at first.  Their
   *  semantics make sense, however, and the trickiness is a side effect of
   *  the requirement that the iterators must be safe.
  */
  template<typename _Iterator>
    class reverse_iterator
    : public iterator<typename iterator_traits<_Iterator>::iterator_category,
		      typename iterator_traits<_Iterator>::value_type,
		      typename iterator_traits<_Iterator>::difference_type,
		      typename iterator_traits<_Iterator>::pointer,
                      typename iterator_traits<_Iterator>::reference>
    {
      template<typename _Iter>
	friend class reverse_iterator;









    protected:
      _Iterator current;

      typedef iterator_traits<_Iterator>		__traits_type;

    public:
      typedef _Iterator					iterator_type;
      typedef typename __traits_type::difference_type	difference_type;
      typedef typename __traits_type::pointer		pointer;
      typedef typename __traits_type::reference		reference;


      /**
       *  The default constructor value-initializes member @p current.
       *  If it is a pointer, that means it is zero-initialized.
      */
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 235 No specification of default ctor for reverse_iterator
      // 1012. reverse_iterator default ctor should value initialize
      _GLIBCXX17_CONSTEXPR
      reverse_iterator() : current() { }

      /**
       *  This %iterator will move in the opposite direction that @p x does.
      */
      explicit _GLIBCXX17_CONSTEXPR
      reverse_iterator(iterator_type __x) : current(__x) { }

      /**
       *  The copy constructor is normal.
      */
      _GLIBCXX17_CONSTEXPR
      reverse_iterator(const reverse_iterator& __x)
      : current(__x.current) { }


      reverse_iterator& operator=(const reverse_iterator&) = default;


      /**
       *  A %reverse_iterator across other types can be copied if the
       *  underlying %iterator can be converted to the type of @c current.
      */
      template<typename _Iter>



	_GLIBCXX17_CONSTEXPR
        reverse_iterator(const reverse_iterator<_Iter>& __x)
	: current(__x.current) { }


      template<typename _Iter>




	_GLIBCXX17_CONSTEXPR
	reverse_iterator&
	operator=(const reverse_iterator<_Iter>& __x)
	{
	  current = __x.current;
	  return *this;
	}


      /**
       *  @return  @c current, the %iterator used for underlying work.
      */
      _GLIBCXX17_CONSTEXPR iterator_type
      base() const
      { return current; }

      /**
       *  @return  A reference to the value at @c --current
       *
       *  This requires that @c --current is dereferenceable.
       *
       *  @warning This implementation requires that for an iterator of the
       *           underlying iterator type, @c x, a reference obtained by
       *           @c *x remains valid after @c x has been modified or
       *           destroyed. This is a bug: http://gcc.gnu.org/PR51823
      */
      _GLIBCXX17_CONSTEXPR reference
      operator*() const
      {
	_Iterator __tmp = current;
	return *--__tmp;
      }

      /**
       *  @return  A pointer to the value at @c --current
       *
       *  This requires that @c --current is dereferenceable.
      */
      _GLIBCXX17_CONSTEXPR pointer
      operator->() const




      {
	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// 1052. operator-> should also support smart pointers
	_Iterator __tmp = current;
	--__tmp;
	return _S_to_pointer(__tmp);
      }

      /**
       *  @return  @c *this
       *
       *  Decrements the underlying iterator.
      */
      _GLIBCXX17_CONSTEXPR reverse_iterator&
      operator++()
      {
	--current;
	return *this;
      }

      /**
       *  @return  The original value of @c *this
       *
       *  Decrements the underlying iterator.
      */
      _GLIBCXX17_CONSTEXPR reverse_iterator
      operator++(int)
      {
	reverse_iterator __tmp = *this;
	--current;
	return __tmp;
      }

      /**
       *  @return  @c *this
       *
       *  Increments the underlying iterator.
      */
      _GLIBCXX17_CONSTEXPR reverse_iterator&
      operator--()
      {
	++current;
	return *this;
      }

      /**
       *  @return  A reverse_iterator with the previous value of @c *this
       *
       *  Increments the underlying iterator.
      */
      _GLIBCXX17_CONSTEXPR reverse_iterator
      operator--(int)
      {
	reverse_iterator __tmp = *this;
	++current;
	return __tmp;
      }

      /**
       *  @return  A reverse_iterator that refers to @c current - @a __n
       *
       *  The underlying iterator must be a Random Access Iterator.
      */
      _GLIBCXX17_CONSTEXPR reverse_iterator
      operator+(difference_type __n) const
      { return reverse_iterator(current - __n); }

      /**
       *  @return  *this
       *
       *  Moves the underlying iterator backwards @a __n steps.
       *  The underlying iterator must be a Random Access Iterator.
      */
      _GLIBCXX17_CONSTEXPR reverse_iterator&
      operator+=(difference_type __n)
      {
	current -= __n;
	return *this;
      }

      /**
       *  @return  A reverse_iterator that refers to @c current - @a __n
       *
       *  The underlying iterator must be a Random Access Iterator.
      */
      _GLIBCXX17_CONSTEXPR reverse_iterator
      operator-(difference_type __n) const
      { return reverse_iterator(current + __n); }

      /**
       *  @return  *this
       *
       *  Moves the underlying iterator forwards @a __n steps.
       *  The underlying iterator must be a Random Access Iterator.
      */
      _GLIBCXX17_CONSTEXPR reverse_iterator&
      operator-=(difference_type __n)
      {
	current += __n;
	return *this;
      }

      /**
       *  @return  The value at @c current - @a __n - 1
       *
       *  The underlying iterator must be a Random Access Iterator.
      */
      _GLIBCXX17_CONSTEXPR reference
      operator[](difference_type __n) const
      { return *(*this + __n); }


    private:
      template<typename _Tp>
	static _GLIBCXX17_CONSTEXPR _Tp*
	_S_to_pointer(_Tp* __p)
        { return __p; }

      template<typename _Tp>
	static _GLIBCXX17_CONSTEXPR pointer
	_S_to_pointer(_Tp __t)
        { return __t.operator->(); }
    };

  //@{
  /**
   *  @param  __x  A %reverse_iterator.
   *  @param  __y  A %reverse_iterator.
   *  @return  A simple bool.
   *
   *  Reverse iterators forward comparisons to their underlying base()
   *  iterators.
   *
  */

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR bool
    operator==(const reverse_iterator<_Iterator>& __x,
	       const reverse_iterator<_Iterator>& __y)
    { return __x.base() == __y.base(); }

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR bool
    operator<(const reverse_iterator<_Iterator>& __x,
	      const reverse_iterator<_Iterator>& __y)
    { return __y.base() < __x.base(); }

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR bool
    operator!=(const reverse_iterator<_Iterator>& __x,
	       const reverse_iterator<_Iterator>& __y)
    { return !(__x == __y); }

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR bool
    operator>(const reverse_iterator<_Iterator>& __x,
	      const reverse_iterator<_Iterator>& __y)
    { return __y < __x; }

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR bool
    operator<=(const reverse_iterator<_Iterator>& __x,
	       const reverse_iterator<_Iterator>& __y)
    { return !(__y < __x); }

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR bool
    operator>=(const reverse_iterator<_Iterator>& __x,
	       const reverse_iterator<_Iterator>& __y)
    { return !(__x < __y); }

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // DR 280. Comparison of reverse_iterator to const reverse_iterator.

  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR bool
    operator==(const reverse_iterator<_IteratorL>& __x,
	       const reverse_iterator<_IteratorR>& __y)
    { return __x.base() == __y.base(); }

  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR bool
    operator<(const reverse_iterator<_IteratorL>& __x,
	      const reverse_iterator<_IteratorR>& __y)
    { return __x.base() > __y.base(); }

  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR bool
    operator!=(const reverse_iterator<_IteratorL>& __x,
	       const reverse_iterator<_IteratorR>& __y)
    { return __x.base() != __y.base(); }

  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR bool
    operator>(const reverse_iterator<_IteratorL>& __x,
	      const reverse_iterator<_IteratorR>& __y)
    { return __x.base() < __y.base(); }

  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR bool
    operator<=(const reverse_iterator<_IteratorL>& __x,
	       const reverse_iterator<_IteratorR>& __y)
    { return __x.base() >= __y.base(); }

  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR bool
    operator>=(const reverse_iterator<_IteratorL>& __x,
	       const reverse_iterator<_IteratorR>& __y)
    { return __x.base() <= __y.base(); }
  //@}

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // DR 685. reverse_iterator/move_iterator difference has invalid signatures
  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR auto
    operator-(const reverse_iterator<_IteratorL>& __x,
	      const reverse_iterator<_IteratorR>& __y)
    -> decltype(__y.base() - __x.base())
    { return __y.base() - __x.base(); }


  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR reverse_iterator<_Iterator>
    operator+(typename reverse_iterator<_Iterator>::difference_type __n,
	      const reverse_iterator<_Iterator>& __x)
    { return reverse_iterator<_Iterator>(__x.base() - __n); }


  // Same as C++14 make_reverse_iterator but used in C++11 mode too.
  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR reverse_iterator<_Iterator>
    __make_reverse_iterator(_Iterator __i)
    { return reverse_iterator<_Iterator>(__i); }


#define __cpp_lib_make_reverse_iterator 201402

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // DR 2285. make_reverse_iterator
  /// Generator function for reverse_iterator.
  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR reverse_iterator<_Iterator>
    make_reverse_iterator(_Iterator __i)
    { return reverse_iterator<_Iterator>(__i); }










  template<typename _Iterator>
    _GLIBCXX20_CONSTEXPR
    auto
    __niter_base(reverse_iterator<_Iterator> __it)
    -> decltype(__make_reverse_iterator(__niter_base(__it.base())))
    { return __make_reverse_iterator(__niter_base(__it.base())); }

  template<typename _Iterator>
    struct __is_move_iterator<reverse_iterator<_Iterator> >
      : __is_move_iterator<_Iterator>
    { };

  template<typename _Iterator>
    _GLIBCXX20_CONSTEXPR
    auto
    __miter_base(reverse_iterator<_Iterator> __it)
    -> decltype(__make_reverse_iterator(__miter_base(__it.base())))
    { return __make_reverse_iterator(__miter_base(__it.base())); }


  // 24.4.2.2.1 back_insert_iterator
  /**
   *  @brief  Turns assignment into insertion.
   *
   *  These are output iterators, constructed from a container-of-T.
   *  Assigning a T to the iterator appends it to the container using
   *  push_back.
   *
   *  Tip:  Using the back_inserter function to create these iterators can
   *  save typing.
  */
  template<typename _Container>
    class back_insert_iterator
    : public iterator<output_iterator_tag, void, void, void, void>
    {
    protected:
      _Container* container;

    public:
      /// A nested typedef for the type of whatever container you used.
      typedef _Container          container_type;






      /// The only way to create this %iterator is with a container.
      explicit _GLIBCXX20_CONSTEXPR
      back_insert_iterator(_Container& __x)
      : container(std::__addressof(__x)) { }

      /**
       *  @param  __value  An instance of whatever type
       *                 container_type::const_reference is; presumably a
       *                 reference-to-const T for container<T>.
       *  @return  This %iterator, for chained operations.
       *
       *  This kind of %iterator doesn't really have a @a position in the
       *  container (you can think of the position as being permanently at
       *  the end, if you like).  Assigning a value to the %iterator will
       *  always append the value to the end of the container.
      */
      _GLIBCXX20_CONSTEXPR
      back_insert_iterator&
      operator=(const typename _Container::value_type& __value)
      {
	container->push_back(__value);
	return *this;
      }

      _GLIBCXX20_CONSTEXPR
      back_insert_iterator&
      operator=(typename _Container::value_type&& __value)
      {
	container->push_back(std::move(__value));
	return *this;
      }


      /// Simply returns *this.
      _GLIBCXX20_CONSTEXPR
      back_insert_iterator&
      operator*()
      { return *this; }

      /// Simply returns *this.  (This %iterator does not @a move.)
      _GLIBCXX20_CONSTEXPR
      back_insert_iterator&
      operator++()
      { return *this; }

      /// Simply returns *this.  (This %iterator does not @a move.)
      _GLIBCXX20_CONSTEXPR
      back_insert_iterator
      operator++(int)
      { return *this; }
    };

  /**
   *  @param  __x  A container of arbitrary type.
   *  @return  An instance of back_insert_iterator working on @p __x.
   *
   *  This wrapper function helps in creating back_insert_iterator instances.
   *  Typing the name of the %iterator requires knowing the precise full
   *  type of the container, which can be tedious and impedes generic
   *  programming.  Using this function lets you take advantage of automatic
   *  template parameter deduction, making the compiler match the correct
   *  types for you.
  */
  template<typename _Container>
    _GLIBCXX20_CONSTEXPR
    inline back_insert_iterator<_Container>
    back_inserter(_Container& __x)
    { return back_insert_iterator<_Container>(__x); }

  /**
   *  @brief  Turns assignment into insertion.
   *
   *  These are output iterators, constructed from a container-of-T.
   *  Assigning a T to the iterator prepends it to the container using
   *  push_front.
   *
   *  Tip:  Using the front_inserter function to create these iterators can
   *  save typing.
  */
  template<typename _Container>
    class front_insert_iterator
    : public iterator<output_iterator_tag, void, void, void, void>
    {
    protected:
      _Container* container;

    public:
      /// A nested typedef for the type of whatever container you used.
      typedef _Container          container_type;






      /// The only way to create this %iterator is with a container.
      explicit _GLIBCXX20_CONSTEXPR
      front_insert_iterator(_Container& __x)
      : container(std::__addressof(__x)) { }

      /**
       *  @param  __value  An instance of whatever type
       *                 container_type::const_reference is; presumably a
       *                 reference-to-const T for container<T>.
       *  @return  This %iterator, for chained operations.
       *
       *  This kind of %iterator doesn't really have a @a position in the
       *  container (you can think of the position as being permanently at
       *  the front, if you like).  Assigning a value to the %iterator will
       *  always prepend the value to the front of the container.
      */
      _GLIBCXX20_CONSTEXPR
      front_insert_iterator&
      operator=(const typename _Container::value_type& __value)
      {
	container->push_front(__value);
	return *this;
      }

      _GLIBCXX20_CONSTEXPR
      front_insert_iterator&
      operator=(typename _Container::value_type&& __value)
      {
	container->push_front(std::move(__value));
	return *this;
      }


      /// Simply returns *this.
      _GLIBCXX20_CONSTEXPR
      front_insert_iterator&
      operator*()
      { return *this; }

      /// Simply returns *this.  (This %iterator does not @a move.)
      _GLIBCXX20_CONSTEXPR
      front_insert_iterator&
      operator++()
      { return *this; }

      /// Simply returns *this.  (This %iterator does not @a move.)
      _GLIBCXX20_CONSTEXPR
      front_insert_iterator
      operator++(int)
      { return *this; }
    };

  /**
   *  @param  __x  A container of arbitrary type.
   *  @return  An instance of front_insert_iterator working on @p x.
   *
   *  This wrapper function helps in creating front_insert_iterator instances.
   *  Typing the name of the %iterator requires knowing the precise full
   *  type of the container, which can be tedious and impedes generic
   *  programming.  Using this function lets you take advantage of automatic
   *  template parameter deduction, making the compiler match the correct
   *  types for you.
  */
  template<typename _Container>
    _GLIBCXX20_CONSTEXPR
    inline front_insert_iterator<_Container>
    front_inserter(_Container& __x)
    { return front_insert_iterator<_Container>(__x); }

  /**
   *  @brief  Turns assignment into insertion.
   *
   *  These are output iterators, constructed from a container-of-T.
   *  Assigning a T to the iterator inserts it in the container at the
   *  %iterator's position, rather than overwriting the value at that
   *  position.
   *
   *  (Sequences will actually insert a @e copy of the value before the
   *  %iterator's position.)
   *
   *  Tip:  Using the inserter function to create these iterators can
   *  save typing.
  */
  template<typename _Container>
    class insert_iterator
    : public iterator<output_iterator_tag, void, void, void, void>
    {







      typedef typename _Container::iterator		_Iter;

    protected:
      _Container* container;
      _Iter iter;


    public:
      /// A nested typedef for the type of whatever container you used.
      typedef _Container          container_type;







      /**
       *  The only way to create this %iterator is with a container and an
       *  initial position (a normal %iterator into the container).
      */
      _GLIBCXX20_CONSTEXPR
      insert_iterator(_Container& __x, _Iter __i)
      : container(std::__addressof(__x)), iter(__i) {}

      /**
       *  @param  __value  An instance of whatever type
       *                 container_type::const_reference is; presumably a
       *                 reference-to-const T for container<T>.
       *  @return  This %iterator, for chained operations.
       *
       *  This kind of %iterator maintains its own position in the
       *  container.  Assigning a value to the %iterator will insert the
       *  value into the container at the place before the %iterator.
       *
       *  The position is maintained such that subsequent assignments will
       *  insert values immediately after one another.  For example,
       *  @code
       *     // vector v contains A and Z
       *
       *     insert_iterator i (v, ++v.begin());
       *     i = 1;
       *     i = 2;
       *     i = 3;
       *
       *     // vector v contains A, 1, 2, 3, and Z
       *  @endcode
      */
      _GLIBCXX20_CONSTEXPR
      insert_iterator&
      operator=(const typename _Container::value_type& __value)
      {
	iter = container->insert(iter, __value);
	++iter;
	return *this;
      }

      _GLIBCXX20_CONSTEXPR
      insert_iterator&
      operator=(typename _Container::value_type&& __value)
      {
	iter = container->insert(iter, std::move(__value));
	++iter;
	return *this;
      }


      /// Simply returns *this.
      _GLIBCXX20_CONSTEXPR
      insert_iterator&
      operator*()
      { return *this; }

      /// Simply returns *this.  (This %iterator does not @a move.)
      _GLIBCXX20_CONSTEXPR
      insert_iterator&
      operator++()
      { return *this; }

      /// Simply returns *this.  (This %iterator does not @a move.)
      _GLIBCXX20_CONSTEXPR
      insert_iterator&
      operator++(int)
      { return *this; }
    };

  /**
   *  @param __x  A container of arbitrary type.
   *  @param __i  An iterator into the container.
   *  @return  An instance of insert_iterator working on @p __x.
   *
   *  This wrapper function helps in creating insert_iterator instances.
   *  Typing the name of the %iterator requires knowing the precise full
   *  type of the container, which can be tedious and impedes generic
   *  programming.  Using this function lets you take advantage of automatic
   *  template parameter deduction, making the compiler match the correct
   *  types for you.
  */






  template<typename _Container>
    inline insert_iterator<_Container>
    inserter(_Container& __x, typename _Container::iterator __i)
    { return insert_iterator<_Container>(__x, __i); }


  // @} group iterators

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // This iterator adapter is @a normal in the sense that it does not
  // change the semantics of any of the operators of its iterator
  // parameter.  Its primary purpose is to convert an iterator that is
  // not a class, e.g. a pointer, into an iterator that is a class.
  // The _Container parameter exists solely so that different containers
  // using this template can instantiate different types, even if the
  // _Iterator parameter is the same.
  template<typename _Iterator, typename _Container>
    class __normal_iterator
    {
    protected:
      _Iterator _M_current;

      typedef std::iterator_traits<_Iterator>		__traits_type;

    public:
      typedef _Iterator					iterator_type;
      typedef typename __traits_type::iterator_category iterator_category;
      typedef typename __traits_type::value_type  	value_type;
      typedef typename __traits_type::difference_type 	difference_type;
      typedef typename __traits_type::reference 	reference;
      typedef typename __traits_type::pointer   	pointer;





      _GLIBCXX_CONSTEXPR __normal_iterator() _GLIBCXX_NOEXCEPT
      : _M_current(_Iterator()) { }

      explicit _GLIBCXX20_CONSTEXPR
      __normal_iterator(const _Iterator& __i) _GLIBCXX_NOEXCEPT
      : _M_current(__i) { }

      // Allow iterator to const_iterator conversion
      template<typename _Iter>
        _GLIBCXX20_CONSTEXPR
        __normal_iterator(const __normal_iterator<_Iter,
			  typename __enable_if<
      	       (std::__are_same<_Iter, typename _Container::pointer>::__value),
		      _Container>::__type>& __i) _GLIBCXX_NOEXCEPT
        : _M_current(__i.base()) { }

      // Forward iterator requirements
      _GLIBCXX20_CONSTEXPR
      reference
      operator*() const _GLIBCXX_NOEXCEPT
      { return *_M_current; }

      _GLIBCXX20_CONSTEXPR
      pointer
      operator->() const _GLIBCXX_NOEXCEPT
      { return _M_current; }

      _GLIBCXX20_CONSTEXPR
      __normal_iterator&
      operator++() _GLIBCXX_NOEXCEPT
      {
	++_M_current;
	return *this;
      }

      _GLIBCXX20_CONSTEXPR
      __normal_iterator
      operator++(int) _GLIBCXX_NOEXCEPT
      { return __normal_iterator(_M_current++); }

      // Bidirectional iterator requirements
      _GLIBCXX20_CONSTEXPR
      __normal_iterator&
      operator--() _GLIBCXX_NOEXCEPT
      {
	--_M_current;
	return *this;
      }

      _GLIBCXX20_CONSTEXPR
      __normal_iterator
      operator--(int) _GLIBCXX_NOEXCEPT
      { return __normal_iterator(_M_current--); }

      // Random access iterator requirements
      _GLIBCXX20_CONSTEXPR
      reference
      operator[](difference_type __n) const _GLIBCXX_NOEXCEPT
      { return _M_current[__n]; }

      _GLIBCXX20_CONSTEXPR
      __normal_iterator&
      operator+=(difference_type __n) _GLIBCXX_NOEXCEPT
      { _M_current += __n; return *this; }

      _GLIBCXX20_CONSTEXPR
      __normal_iterator
      operator+(difference_type __n) const _GLIBCXX_NOEXCEPT
      { return __normal_iterator(_M_current + __n); }

      _GLIBCXX20_CONSTEXPR
      __normal_iterator&
      operator-=(difference_type __n) _GLIBCXX_NOEXCEPT
      { _M_current -= __n; return *this; }

      _GLIBCXX20_CONSTEXPR
      __normal_iterator
      operator-(difference_type __n) const _GLIBCXX_NOEXCEPT
      { return __normal_iterator(_M_current - __n); }

      _GLIBCXX20_CONSTEXPR
      const _Iterator&
      base() const _GLIBCXX_NOEXCEPT
      { return _M_current; }
    };

  // Note: In what follows, the left- and right-hand-side iterators are
  // allowed to vary in types (conceptually in cv-qualification) so that
  // comparison between cv-qualified and non-cv-qualified iterators be
  // valid.  However, the greedy and unfriendly operators in std::rel_ops
  // will make overload resolution ambiguous (when in scope) if we don't
  // provide overloads whose operands are of the same type.  Can someone
  // remind me what generic programming is about? -- Gaby

   // Forward iterator requirements
  template<typename _IteratorL, typename _IteratorR, typename _Container>
    _GLIBCXX20_CONSTEXPR
    inline bool
    operator==(const __normal_iterator<_IteratorL, _Container>& __lhs,
	       const __normal_iterator<_IteratorR, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() == __rhs.base(); }

  template<typename _Iterator, typename _Container>
    _GLIBCXX20_CONSTEXPR
    inline bool
    operator==(const __normal_iterator<_Iterator, _Container>& __lhs,
	       const __normal_iterator<_Iterator, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() == __rhs.base(); }

  template<typename _IteratorL, typename _IteratorR, typename _Container>
    _GLIBCXX20_CONSTEXPR
    inline bool
    operator!=(const __normal_iterator<_IteratorL, _Container>& __lhs,
	       const __normal_iterator<_IteratorR, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() != __rhs.base(); }

  template<typename _Iterator, typename _Container>
    _GLIBCXX20_CONSTEXPR
    inline bool
    operator!=(const __normal_iterator<_Iterator, _Container>& __lhs,
	       const __normal_iterator<_Iterator, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() != __rhs.base(); }

  // Random access iterator requirements
  template<typename _IteratorL, typename _IteratorR, typename _Container>
    inline bool
    operator<(const __normal_iterator<_IteratorL, _Container>& __lhs,
	      const __normal_iterator<_IteratorR, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() < __rhs.base(); }

  template<typename _Iterator, typename _Container>
    _GLIBCXX20_CONSTEXPR
    inline bool
    operator<(const __normal_iterator<_Iterator, _Container>& __lhs,
	      const __normal_iterator<_Iterator, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() < __rhs.base(); }

  template<typename _IteratorL, typename _IteratorR, typename _Container>
    inline bool
    operator>(const __normal_iterator<_IteratorL, _Container>& __lhs,
	      const __normal_iterator<_IteratorR, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() > __rhs.base(); }

  template<typename _Iterator, typename _Container>
    _GLIBCXX20_CONSTEXPR
    inline bool
    operator>(const __normal_iterator<_Iterator, _Container>& __lhs,
	      const __normal_iterator<_Iterator, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() > __rhs.base(); }

  template<typename _IteratorL, typename _IteratorR, typename _Container>
    inline bool
    operator<=(const __normal_iterator<_IteratorL, _Container>& __lhs,
	       const __normal_iterator<_IteratorR, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() <= __rhs.base(); }

  template<typename _Iterator, typename _Container>
    _GLIBCXX20_CONSTEXPR
    inline bool
    operator<=(const __normal_iterator<_Iterator, _Container>& __lhs,
	       const __normal_iterator<_Iterator, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() <= __rhs.base(); }

  template<typename _IteratorL, typename _IteratorR, typename _Container>
    inline bool
    operator>=(const __normal_iterator<_IteratorL, _Container>& __lhs,
	       const __normal_iterator<_IteratorR, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() >= __rhs.base(); }

  template<typename _Iterator, typename _Container>
    _GLIBCXX20_CONSTEXPR
    inline bool
    operator>=(const __normal_iterator<_Iterator, _Container>& __lhs,
	       const __normal_iterator<_Iterator, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() >= __rhs.base(); }


  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // According to the resolution of DR179 not only the various comparison
  // operators but also operator- must accept mixed iterator/const_iterator
  // parameters.
  template<typename _IteratorL, typename _IteratorR, typename _Container>

    // DR 685.
    _GLIBCXX20_CONSTEXPR
    inline auto
    operator-(const __normal_iterator<_IteratorL, _Container>& __lhs,
	      const __normal_iterator<_IteratorR, _Container>& __rhs) noexcept
    -> decltype(__lhs.base() - __rhs.base())





    { return __lhs.base() - __rhs.base(); }

  template<typename _Iterator, typename _Container>
    _GLIBCXX20_CONSTEXPR
    inline typename __normal_iterator<_Iterator, _Container>::difference_type
    operator-(const __normal_iterator<_Iterator, _Container>& __lhs,
	      const __normal_iterator<_Iterator, _Container>& __rhs)
    _GLIBCXX_NOEXCEPT
    { return __lhs.base() - __rhs.base(); }

  template<typename _Iterator, typename _Container>
    _GLIBCXX20_CONSTEXPR
    inline __normal_iterator<_Iterator, _Container>
    operator+(typename __normal_iterator<_Iterator, _Container>::difference_type
	      __n, const __normal_iterator<_Iterator, _Container>& __i)
    _GLIBCXX_NOEXCEPT
    { return __normal_iterator<_Iterator, _Container>(__i.base() + __n); }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<typename _Iterator, typename _Container>
    _GLIBCXX20_CONSTEXPR
    _Iterator
    __niter_base(__gnu_cxx::__normal_iterator<_Iterator, _Container> __it)
    _GLIBCXX_NOEXCEPT_IF(std::is_nothrow_copy_constructible<_Iterator>::value)
    { return __it.base(); }


  /**
   * @addtogroup iterators
   * @{
   */


  // 24.4.3  Move iterators
  /**
   *  Class template move_iterator is an iterator adapter with the same
   *  behavior as the underlying iterator except that its dereference
   *  operator implicitly converts the value returned by the underlying
   *  iterator's dereference operator to an rvalue reference.  Some
   *  generic algorithms can be called with move iterators to replace
   *  copying with moving.
   */
  template<typename _Iterator>
    class move_iterator
    {
      _Iterator _M_current;

      using __traits_type = iterator_traits<_Iterator>;



      using __base_ref = typename __traits_type::reference;


      template<typename _Iter2>
	friend class move_iterator;









    public:
      using iterator_type = _Iterator;

      typedef typename __traits_type::iterator_category iterator_category;
      typedef typename __traits_type::value_type  	value_type;
      typedef typename __traits_type::difference_type	difference_type;
      // NB: DR 680.
      typedef _Iterator					pointer;
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 2106. move_iterator wrapping iterators returning prvalues
      typedef typename conditional<is_reference<__base_ref>::value,
			 typename remove_reference<__base_ref>::type&&,
			 __base_ref>::type		reference;


      _GLIBCXX17_CONSTEXPR
      move_iterator()
      : _M_current() { }

      explicit _GLIBCXX17_CONSTEXPR
      move_iterator(iterator_type __i)
      : _M_current(std::move(__i)) { }

      template<typename _Iter>



	_GLIBCXX17_CONSTEXPR
	move_iterator(const move_iterator<_Iter>& __i)
	: _M_current(__i._M_current) { }

      template<typename _Iter>




	_GLIBCXX17_CONSTEXPR
	move_iterator& operator=(const move_iterator<_Iter>& __i)
	{
	  _M_current = __i._M_current;
	  return *this;
	}


      _GLIBCXX17_CONSTEXPR iterator_type
      base() const
      { return _M_current; }

      _GLIBCXX17_CONSTEXPR reference
      operator*() const



      { return static_cast<reference>(*_M_current); }


      _GLIBCXX17_CONSTEXPR pointer
      operator->() const
      { return _M_current; }

      _GLIBCXX17_CONSTEXPR move_iterator&
      operator++()
      {
	++_M_current;
	return *this;
      }

      _GLIBCXX17_CONSTEXPR move_iterator
      operator++(int)
      {
	move_iterator __tmp = *this;
	++_M_current;
	return __tmp;
      }







      _GLIBCXX17_CONSTEXPR move_iterator&
      operator--()
      {
	--_M_current;
	return *this;
      }

      _GLIBCXX17_CONSTEXPR move_iterator
      operator--(int)
      {
	move_iterator __tmp = *this;
	--_M_current;
	return __tmp;
      }

      _GLIBCXX17_CONSTEXPR move_iterator
      operator+(difference_type __n) const
      { return move_iterator(_M_current + __n); }

      _GLIBCXX17_CONSTEXPR move_iterator&
      operator+=(difference_type __n)
      {
	_M_current += __n;
	return *this;
      }

      _GLIBCXX17_CONSTEXPR move_iterator
      operator-(difference_type __n) const
      { return move_iterator(_M_current - __n); }
    
      _GLIBCXX17_CONSTEXPR move_iterator&
      operator-=(difference_type __n)
      { 
	_M_current -= __n;
	return *this;
      }

      _GLIBCXX17_CONSTEXPR reference
      operator[](difference_type __n) const



      { return std::move(_M_current[__n]); }


    };

  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR bool
    operator==(const move_iterator<_IteratorL>& __x,
	       const move_iterator<_IteratorR>& __y)



    { return __x.base() == __y.base(); }

  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR bool
    operator!=(const move_iterator<_IteratorL>& __x,
	       const move_iterator<_IteratorR>& __y)
    { return !(__x == __y); }


  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR bool
    operator<(const move_iterator<_IteratorL>& __x,
	      const move_iterator<_IteratorR>& __y)



    { return __x.base() < __y.base(); }

  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR bool
    operator<=(const move_iterator<_IteratorL>& __x,
	       const move_iterator<_IteratorR>& __y)



    { return !(__y < __x); }

  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR bool
    operator>(const move_iterator<_IteratorL>& __x,
	      const move_iterator<_IteratorR>& __y)



    { return __y < __x; }

  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR bool
    operator>=(const move_iterator<_IteratorL>& __x,
	       const move_iterator<_IteratorR>& __y)



    { return !(__x < __y); }


  // Note: See __normal_iterator operators note from Gaby to understand
  // why we have these extra overloads for some move_iterator operators.

  // These extra overloads are not needed in C++20, because the ones above
  // are constrained with a requires-clause and so overload resolution will
  // prefer them to greedy unconstrained function templates.

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR bool
    operator==(const move_iterator<_Iterator>& __x,
	       const move_iterator<_Iterator>& __y)
    { return __x.base() == __y.base(); }

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR bool
    operator!=(const move_iterator<_Iterator>& __x,
	       const move_iterator<_Iterator>& __y)
    { return !(__x == __y); }

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR bool
    operator<(const move_iterator<_Iterator>& __x,
	      const move_iterator<_Iterator>& __y)
    { return __x.base() < __y.base(); }

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR bool
    operator<=(const move_iterator<_Iterator>& __x,
	       const move_iterator<_Iterator>& __y)
    { return !(__y < __x); }

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR bool
    operator>(const move_iterator<_Iterator>& __x,
	      const move_iterator<_Iterator>& __y)
    { return __y < __x; }

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR bool
    operator>=(const move_iterator<_Iterator>& __x,
	       const move_iterator<_Iterator>& __y)
    { return !(__x < __y); }


  // DR 685.
  template<typename _IteratorL, typename _IteratorR>
    inline _GLIBCXX17_CONSTEXPR auto
    operator-(const move_iterator<_IteratorL>& __x,
	      const move_iterator<_IteratorR>& __y)
    -> decltype(__x.base() - __y.base())
    { return __x.base() - __y.base(); }

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR move_iterator<_Iterator>
    operator+(typename move_iterator<_Iterator>::difference_type __n,
	      const move_iterator<_Iterator>& __x)
    { return __x + __n; }

  template<typename _Iterator>
    inline _GLIBCXX17_CONSTEXPR move_iterator<_Iterator>
    make_move_iterator(_Iterator __i)
    { return move_iterator<_Iterator>(std::move(__i)); }

  template<typename _Iterator, typename _ReturnType
    = typename conditional<__move_if_noexcept_cond
      <typename iterator_traits<_Iterator>::value_type>::value,
                _Iterator, move_iterator<_Iterator>>::type>
    inline _GLIBCXX17_CONSTEXPR _ReturnType
    __make_move_if_noexcept_iterator(_Iterator __i)
    { return _ReturnType(__i); }

  // Overload for pointers that matches std::move_if_noexcept more closely,
  // returning a constant iterator when we don't want to move.
  template<typename _Tp, typename _ReturnType
    = typename conditional<__move_if_noexcept_cond<_Tp>::value,
			   const _Tp*, move_iterator<_Tp*>>::type>
    inline _GLIBCXX17_CONSTEXPR _ReturnType
    __make_move_if_noexcept_iterator(_Tp* __i)
    { return _ReturnType(__i); }


  // @} group iterators

  template<typename _Iterator>
    auto
    __niter_base(move_iterator<_Iterator> __it)
    -> decltype(make_move_iterator(__niter_base(__it.base())))
    { return make_move_iterator(__niter_base(__it.base())); }

  template<typename _Iterator>
    struct __is_move_iterator<move_iterator<_Iterator> >
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };

  template<typename _Iterator>
    auto
    __miter_base(move_iterator<_Iterator> __it)
    -> decltype(__miter_base(__it.base()))
    { return __miter_base(__it.base()); }

#define _GLIBCXX_MAKE_MOVE_ITERATOR(_Iter) std::make_move_iterator(_Iter)
#define _GLIBCXX_MAKE_MOVE_IF_NOEXCEPT_ITERATOR(_Iter) std::__make_move_if_noexcept_iterator(_Iter)







  // These helper traits are used for deduction guides
  // of associative containers.
  template<typename _InputIterator>
    using __iter_key_t = remove_const_t<
    typename iterator_traits<_InputIterator>::value_type::first_type>;

  template<typename _InputIterator>
    using __iter_val_t =
    typename iterator_traits<_InputIterator>::value_type::second_type;

  template<typename _T1, typename _T2>
    struct pair;

  template<typename _InputIterator>
    using __iter_to_alloc_t =
    pair<add_const_t<__iter_key_t<_InputIterator>>,
	 __iter_val_t<_InputIterator>>;


_GLIBCXX_END_NAMESPACE_VERSION
} // namespace






// Debugging support implementation -*- C++ -*-

// Copyright (C) 2003-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file debug/debug.h
 *  This file is a GNU debug extension to the Standard C++ Library.
 */


#define _GLIBCXX_DEBUG_MACRO_SWITCH_H 1

/** Macros and namespaces used by the implementation outside of debug
 *  wrappers to verify certain properties. The __glibcxx_requires_xxx
 *  macros are merely wrappers around the __glibcxx_check_xxx wrappers
 *  when we are compiling with debug mode, but disappear when we are
 *  in release mode so that there is no checking performed in, e.g.,
 *  the standard library algorithms.
*/



// Debug mode namespaces.

/**
 * @namespace std::__debug
 * @brief GNU debug code, replaces standard behavior with debug behavior.
 */
namespace std
{ 
  namespace __debug { } 
}

/** @namespace __gnu_debug
 *  @brief GNU debug classes for public use.
*/
namespace __gnu_debug
{
  using namespace std::__debug;

  template<typename _Ite, typename _Seq, typename _Cat>
    struct _Safe_iterator;
}



#define __glibcxx_requires_cond(_Cond,_Msg) 
#define __glibcxx_requires_valid_range(_First,_Last) 
#define __glibcxx_requires_can_increment(_First,_Size) 
#define __glibcxx_requires_can_increment_range(_First1,_Last1,_First2) 
#define __glibcxx_requires_can_decrement_range(_First1,_Last1,_First2) 
#define __glibcxx_requires_sorted(_First,_Last) 
#define __glibcxx_requires_sorted_pred(_First,_Last,_Pred) 
#define __glibcxx_requires_sorted_set(_First1,_Last1,_First2) 
#define __glibcxx_requires_sorted_set_pred(_First1,_Last1,_First2,_Pred) 
#define __glibcxx_requires_partitioned_lower(_First,_Last,_Value) 
#define __glibcxx_requires_partitioned_upper(_First,_Last,_Value) 
#define __glibcxx_requires_partitioned_lower_pred(_First,_Last,_Value,_Pred) 
#define __glibcxx_requires_partitioned_upper_pred(_First,_Last,_Value,_Pred) 
#define __glibcxx_requires_heap(_First,_Last) 
#define __glibcxx_requires_heap_pred(_First,_Last,_Pred) 
#define __glibcxx_requires_string(_String) 
#define __glibcxx_requires_string_len(_String,_Len) 
#define __glibcxx_requires_irreflexive(_First,_Last) 
#define __glibcxx_requires_irreflexive2(_First,_Last) 
#define __glibcxx_requires_irreflexive_pred(_First,_Last,_Pred) 
#define __glibcxx_requires_irreflexive_pred2(_First,_Last,_Pred) 



// Default predicates for internal use -*- C++ -*-

// Copyright (C) 2013-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file predefined_ops.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly. @headername{algorithm}
 */


#define _GLIBCXX_PREDEFINED_OPS_H 1

namespace __gnu_cxx
{
namespace __ops
{
  struct _Iter_less_iter
  {
    template<typename _Iterator1, typename _Iterator2>
      _GLIBCXX14_CONSTEXPR
      bool
      operator()(_Iterator1 __it1, _Iterator2 __it2) const
      { return *__it1 < *__it2; }
  };

  _GLIBCXX14_CONSTEXPR
  inline _Iter_less_iter
  __iter_less_iter()
  { return _Iter_less_iter(); }

  struct _Iter_less_val
  {

    constexpr _Iter_less_val() = default;




    _GLIBCXX20_CONSTEXPR
    explicit
    _Iter_less_val(_Iter_less_iter) { }

    template<typename _Iterator, typename _Value>
      _GLIBCXX20_CONSTEXPR
      bool
      operator()(_Iterator __it, _Value& __val) const
      { return *__it < __val; }
  };

  _GLIBCXX20_CONSTEXPR
  inline _Iter_less_val
  __iter_less_val()
  { return _Iter_less_val(); }

  _GLIBCXX20_CONSTEXPR
  inline _Iter_less_val
  __iter_comp_val(_Iter_less_iter)
  { return _Iter_less_val(); }

  struct _Val_less_iter
  {

    constexpr _Val_less_iter() = default;




    _GLIBCXX20_CONSTEXPR
    explicit
    _Val_less_iter(_Iter_less_iter) { }

    template<typename _Value, typename _Iterator>
      _GLIBCXX20_CONSTEXPR
      bool
      operator()(_Value& __val, _Iterator __it) const
      { return __val < *__it; }
  };

  _GLIBCXX20_CONSTEXPR
  inline _Val_less_iter
  __val_less_iter()
  { return _Val_less_iter(); }

  _GLIBCXX20_CONSTEXPR
  inline _Val_less_iter
  __val_comp_iter(_Iter_less_iter)
  { return _Val_less_iter(); }

  struct _Iter_equal_to_iter
  {
    template<typename _Iterator1, typename _Iterator2>
      _GLIBCXX20_CONSTEXPR
      bool
      operator()(_Iterator1 __it1, _Iterator2 __it2) const
      { return *__it1 == *__it2; }
  };

  _GLIBCXX20_CONSTEXPR
  inline _Iter_equal_to_iter
  __iter_equal_to_iter()
  { return _Iter_equal_to_iter(); }

  struct _Iter_equal_to_val
  {
    template<typename _Iterator, typename _Value>
      _GLIBCXX20_CONSTEXPR
      bool
      operator()(_Iterator __it, _Value& __val) const
      { return *__it == __val; }
  };

  _GLIBCXX20_CONSTEXPR
  inline _Iter_equal_to_val
  __iter_equal_to_val()
  { return _Iter_equal_to_val(); }

  _GLIBCXX20_CONSTEXPR
  inline _Iter_equal_to_val
  __iter_comp_val(_Iter_equal_to_iter)
  { return _Iter_equal_to_val(); }

  template<typename _Compare>
    struct _Iter_comp_iter
    {
      _Compare _M_comp;

      explicit _GLIBCXX14_CONSTEXPR
      _Iter_comp_iter(_Compare __comp)
	: _M_comp(_GLIBCXX_MOVE(__comp))
      { }

      template<typename _Iterator1, typename _Iterator2>
        _GLIBCXX14_CONSTEXPR
        bool
        operator()(_Iterator1 __it1, _Iterator2 __it2)
        { return bool(_M_comp(*__it1, *__it2)); }
    };

  template<typename _Compare>
    _GLIBCXX14_CONSTEXPR
    inline _Iter_comp_iter<_Compare>
    __iter_comp_iter(_Compare __comp)
    { return _Iter_comp_iter<_Compare>(_GLIBCXX_MOVE(__comp)); }

  template<typename _Compare>
    struct _Iter_comp_val
    {
      _Compare _M_comp;

      _GLIBCXX20_CONSTEXPR
      explicit
      _Iter_comp_val(_Compare __comp)
	: _M_comp(_GLIBCXX_MOVE(__comp))
      { }

      _GLIBCXX20_CONSTEXPR
      explicit
      _Iter_comp_val(const _Iter_comp_iter<_Compare>& __comp)
	: _M_comp(__comp._M_comp)
      { }


      _GLIBCXX20_CONSTEXPR
      explicit
      _Iter_comp_val(_Iter_comp_iter<_Compare>&& __comp)
	: _M_comp(std::move(__comp._M_comp))
      { }


      template<typename _Iterator, typename _Value>
	_GLIBCXX20_CONSTEXPR
	bool
	operator()(_Iterator __it, _Value& __val)
	{ return bool(_M_comp(*__it, __val)); }
    };

  template<typename _Compare>
    _GLIBCXX20_CONSTEXPR
    inline _Iter_comp_val<_Compare>
    __iter_comp_val(_Compare __comp)
    { return _Iter_comp_val<_Compare>(_GLIBCXX_MOVE(__comp)); }

  template<typename _Compare>
    _GLIBCXX20_CONSTEXPR
    inline _Iter_comp_val<_Compare>
    __iter_comp_val(_Iter_comp_iter<_Compare> __comp)
    { return _Iter_comp_val<_Compare>(_GLIBCXX_MOVE(__comp)); }

  template<typename _Compare>
    struct _Val_comp_iter
    {
      _Compare _M_comp;

      _GLIBCXX20_CONSTEXPR
      explicit
      _Val_comp_iter(_Compare __comp)
	: _M_comp(_GLIBCXX_MOVE(__comp))
      { }

      _GLIBCXX20_CONSTEXPR
      explicit
      _Val_comp_iter(const _Iter_comp_iter<_Compare>& __comp)
	: _M_comp(__comp._M_comp)
      { }


      _GLIBCXX20_CONSTEXPR
      explicit
      _Val_comp_iter(_Iter_comp_iter<_Compare>&& __comp)
	: _M_comp(std::move(__comp._M_comp))
      { }


      template<typename _Value, typename _Iterator>
	_GLIBCXX20_CONSTEXPR
	bool
	operator()(_Value& __val, _Iterator __it)
	{ return bool(_M_comp(__val, *__it)); }
    };

  template<typename _Compare>
    _GLIBCXX20_CONSTEXPR
    inline _Val_comp_iter<_Compare>
    __val_comp_iter(_Compare __comp)
    { return _Val_comp_iter<_Compare>(_GLIBCXX_MOVE(__comp)); }

  template<typename _Compare>
    _GLIBCXX20_CONSTEXPR
    inline _Val_comp_iter<_Compare>
    __val_comp_iter(_Iter_comp_iter<_Compare> __comp)
    { return _Val_comp_iter<_Compare>(_GLIBCXX_MOVE(__comp)); }

  template<typename _Value>
    struct _Iter_equals_val
    {
      _Value& _M_value;

      _GLIBCXX20_CONSTEXPR
      explicit
      _Iter_equals_val(_Value& __value)
	: _M_value(__value)
      { }

      template<typename _Iterator>
	_GLIBCXX20_CONSTEXPR
	bool
	operator()(_Iterator __it)
	{ return *__it == _M_value; }
    };

  template<typename _Value>
    _GLIBCXX20_CONSTEXPR
    inline _Iter_equals_val<_Value>
    __iter_equals_val(_Value& __val)
    { return _Iter_equals_val<_Value>(__val); }

  template<typename _Iterator1>
    struct _Iter_equals_iter
    {
      _Iterator1 _M_it1;

      _GLIBCXX20_CONSTEXPR
      explicit
      _Iter_equals_iter(_Iterator1 __it1)
	: _M_it1(__it1)
      { }

      template<typename _Iterator2>
	_GLIBCXX20_CONSTEXPR
	bool
	operator()(_Iterator2 __it2)
	{ return *__it2 == *_M_it1; }
    };

  template<typename _Iterator>
    _GLIBCXX20_CONSTEXPR
    inline _Iter_equals_iter<_Iterator>
    __iter_comp_iter(_Iter_equal_to_iter, _Iterator __it)
    { return _Iter_equals_iter<_Iterator>(__it); }

  template<typename _Predicate>
    struct _Iter_pred
    {
      _Predicate _M_pred;

      _GLIBCXX20_CONSTEXPR
      explicit
      _Iter_pred(_Predicate __pred)
	: _M_pred(_GLIBCXX_MOVE(__pred))
      { }

      template<typename _Iterator>
	_GLIBCXX20_CONSTEXPR
	bool
	operator()(_Iterator __it)
	{ return bool(_M_pred(*__it)); }
    };

  template<typename _Predicate>
    _GLIBCXX20_CONSTEXPR
    inline _Iter_pred<_Predicate>
    __pred_iter(_Predicate __pred)
    { return _Iter_pred<_Predicate>(_GLIBCXX_MOVE(__pred)); }

  template<typename _Compare, typename _Value>
    struct _Iter_comp_to_val
    {
      _Compare _M_comp;
      _Value& _M_value;

      _GLIBCXX20_CONSTEXPR
      _Iter_comp_to_val(_Compare __comp, _Value& __value)
	: _M_comp(_GLIBCXX_MOVE(__comp)), _M_value(__value)
      { }

      template<typename _Iterator>
	_GLIBCXX20_CONSTEXPR
	bool
	operator()(_Iterator __it)
	{ return bool(_M_comp(*__it, _M_value)); }
    };

  template<typename _Compare, typename _Value>
    _Iter_comp_to_val<_Compare, _Value>
    _GLIBCXX20_CONSTEXPR
    __iter_comp_val(_Compare __comp, _Value &__val)
    {
      return _Iter_comp_to_val<_Compare, _Value>(_GLIBCXX_MOVE(__comp), __val);
    }

  template<typename _Compare, typename _Iterator1>
    struct _Iter_comp_to_iter
    {
      _Compare _M_comp;
      _Iterator1 _M_it1;

      _GLIBCXX20_CONSTEXPR
      _Iter_comp_to_iter(_Compare __comp, _Iterator1 __it1)
	: _M_comp(_GLIBCXX_MOVE(__comp)), _M_it1(__it1)
      { }

      template<typename _Iterator2>
	_GLIBCXX20_CONSTEXPR
	bool
	operator()(_Iterator2 __it2)
	{ return bool(_M_comp(*__it2, *_M_it1)); }
    };

  template<typename _Compare, typename _Iterator>
    _GLIBCXX20_CONSTEXPR
    inline _Iter_comp_to_iter<_Compare, _Iterator>
    __iter_comp_iter(_Iter_comp_iter<_Compare> __comp, _Iterator __it)
    {
      return _Iter_comp_to_iter<_Compare, _Iterator>(
	  _GLIBCXX_MOVE(__comp._M_comp), __it);
    }

  template<typename _Predicate>
    struct _Iter_negate
    {
      _Predicate _M_pred;

      _GLIBCXX20_CONSTEXPR
      explicit
      _Iter_negate(_Predicate __pred)
	: _M_pred(_GLIBCXX_MOVE(__pred))
      { }

      template<typename _Iterator>
	_GLIBCXX20_CONSTEXPR
	bool
	operator()(_Iterator __it)
	{ return !bool(_M_pred(*__it)); }
    };

  template<typename _Predicate>
    _GLIBCXX20_CONSTEXPR
    inline _Iter_negate<_Predicate>
    __negate(_Iter_pred<_Predicate> __pred)
    { return _Iter_negate<_Predicate>(_GLIBCXX_MOVE(__pred._M_pred)); }

} // namespace __ops
} // namespace __gnu_cxx








namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /*
   * A constexpr wrapper for __builtin_memcmp.
   * @param __num The number of elements of type _Tp (not bytes).
   */
  template<typename _Tp, typename _Up>
    _GLIBCXX14_CONSTEXPR
    inline int
    __memcmp(const _Tp* __first1, const _Up* __first2, size_t __num)
    {

      static_assert(sizeof(_Tp) == sizeof(_Up), "can be compared with memcmp");

	return __builtin_memcmp(__first1, __first2, sizeof(_Tp) * __num);
    }


  /**
   *  @brief Swaps the contents of two iterators.
   *  @ingroup mutating_algorithms
   *  @param  __a  An iterator.
   *  @param  __b  Another iterator.
   *  @return   Nothing.
   *
   *  This function swaps the values pointed to by two iterators, not the
   *  iterators themselves.
  */
  template<typename _ForwardIterator1, typename _ForwardIterator2>
    _GLIBCXX20_CONSTEXPR
    inline void
    iter_swap(_ForwardIterator1 __a, _ForwardIterator2 __b)
    {
      // concept requirements
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator1>)
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator2>)

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 187. iter_swap underspecified
      swap(*__a, *__b);

    }

  /**
   *  @brief Swap the elements of two sequences.
   *  @ingroup mutating_algorithms
   *  @param  __first1  A forward iterator.
   *  @param  __last1   A forward iterator.
   *  @param  __first2  A forward iterator.
   *  @return   An iterator equal to @p first2+(last1-first1).
   *
   *  Swaps each element in the range @p [first1,last1) with the
   *  corresponding element in the range @p [first2,(last1-first1)).
   *  The ranges must not overlap.
  */
  template<typename _ForwardIterator1, typename _ForwardIterator2>
    _GLIBCXX20_CONSTEXPR
    _ForwardIterator2
    swap_ranges(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
		_ForwardIterator2 __first2)
    {
      // concept requirements
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator1>)
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator2>)
      __glibcxx_requires_valid_range(__first1, __last1);

      for (; __first1 != __last1; ++__first1, (void)++__first2)
	std::iter_swap(__first1, __first2);
      return __first2;
    }

  /**
   *  @brief This does what you think it does.
   *  @ingroup sorting_algorithms
   *  @param  __a  A thing of arbitrary type.
   *  @param  __b  Another thing of arbitrary type.
   *  @return   The lesser of the parameters.
   *
   *  This is the simple classic generic implementation.  It will work on
   *  temporary expressions, since they are only evaluated once, unlike a
   *  preprocessor macro.
  */
  template<typename _Tp>
    _GLIBCXX14_CONSTEXPR
    inline const _Tp&
    min(const _Tp& __a, const _Tp& __b)
    {
      // concept requirements
      __glibcxx_function_requires(_LessThanComparableConcept<_Tp>)
      //return __b < __a ? __b : __a;
      if (__b < __a)
	return __b;
      return __a;
    }

  /**
   *  @brief This does what you think it does.
   *  @ingroup sorting_algorithms
   *  @param  __a  A thing of arbitrary type.
   *  @param  __b  Another thing of arbitrary type.
   *  @return   The greater of the parameters.
   *
   *  This is the simple classic generic implementation.  It will work on
   *  temporary expressions, since they are only evaluated once, unlike a
   *  preprocessor macro.
  */
  template<typename _Tp>
    _GLIBCXX14_CONSTEXPR
    inline const _Tp&
    max(const _Tp& __a, const _Tp& __b)
    {
      // concept requirements
      __glibcxx_function_requires(_LessThanComparableConcept<_Tp>)
      //return  __a < __b ? __b : __a;
      if (__a < __b)
	return __b;
      return __a;
    }

  /**
   *  @brief This does what you think it does.
   *  @ingroup sorting_algorithms
   *  @param  __a  A thing of arbitrary type.
   *  @param  __b  Another thing of arbitrary type.
   *  @param  __comp  A @link comparison_functors comparison functor@endlink.
   *  @return   The lesser of the parameters.
   *
   *  This will work on temporary expressions, since they are only evaluated
   *  once, unlike a preprocessor macro.
  */
  template<typename _Tp, typename _Compare>
    _GLIBCXX14_CONSTEXPR
    inline const _Tp&
    min(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      //return __comp(__b, __a) ? __b : __a;
      if (__comp(__b, __a))
	return __b;
      return __a;
    }

  /**
   *  @brief This does what you think it does.
   *  @ingroup sorting_algorithms
   *  @param  __a  A thing of arbitrary type.
   *  @param  __b  Another thing of arbitrary type.
   *  @param  __comp  A @link comparison_functors comparison functor@endlink.
   *  @return   The greater of the parameters.
   *
   *  This will work on temporary expressions, since they are only evaluated
   *  once, unlike a preprocessor macro.
  */
  template<typename _Tp, typename _Compare>
    _GLIBCXX14_CONSTEXPR
    inline const _Tp&
    max(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      //return __comp(__a, __b) ? __b : __a;
      if (__comp(__a, __b))
	return __b;
      return __a;
    }

  // Fallback implementation of the function in bits/stl_iterator.h used to
  // remove the __normal_iterator wrapper. See copy, fill, ...
  template<typename _Iterator>
    _GLIBCXX20_CONSTEXPR
    inline _Iterator
    __niter_base(_Iterator __it)
    _GLIBCXX_NOEXCEPT_IF(std::is_nothrow_copy_constructible<_Iterator>::value)
    { return __it; }

  template<typename _Ite, typename _Seq>
    _Ite
    __niter_base(const ::__gnu_debug::_Safe_iterator<_Ite, _Seq,
		 std::random_access_iterator_tag>&);

  // Reverse the __niter_base transformation to get a
  // __normal_iterator back again (this assumes that __normal_iterator
  // is only used to wrap random access iterators, like pointers).
  template<typename _From, typename _To>
    _GLIBCXX20_CONSTEXPR
    inline _From
    __niter_wrap(_From __from, _To __res)
    { return __from + (__res - std::__niter_base(__from)); }

  // No need to wrap, iterator already has the right type.
  template<typename _Iterator>
    _GLIBCXX20_CONSTEXPR
    inline _Iterator
    __niter_wrap(const _Iterator&, _Iterator __res)
    { return __res; }

  // All of these auxiliary structs serve two purposes.  (1) Replace
  // calls to copy with memmove whenever possible.  (Memmove, not memcpy,
  // because the input and output ranges are permitted to overlap.)
  // (2) If we're using random access iterators, then write the loop as
  // a for loop with an explicit count.

  template<bool _IsMove, bool _IsSimple, typename _Category>
    struct __copy_move
    {
      template<typename _II, typename _OI>
	_GLIBCXX20_CONSTEXPR
	static _OI
	__copy_m(_II __first, _II __last, _OI __result)
	{
	  for (; __first != __last; ++__result, (void)++__first)
	    *__result = *__first;
	  return __result;
	}
    };


  template<typename _Category>
    struct __copy_move<true, false, _Category>
    {
      template<typename _II, typename _OI>
	_GLIBCXX20_CONSTEXPR
	static _OI
	__copy_m(_II __first, _II __last, _OI __result)
	{
	  for (; __first != __last; ++__result, (void)++__first)
	    *__result = std::move(*__first);
	  return __result;
	}
    };


  template<>
    struct __copy_move<false, false, random_access_iterator_tag>
    {
      template<typename _II, typename _OI>
	_GLIBCXX20_CONSTEXPR
	static _OI
	__copy_m(_II __first, _II __last, _OI __result)
	{
	  typedef typename iterator_traits<_II>::difference_type _Distance;
	  for(_Distance __n = __last - __first; __n > 0; --__n)
	    {
	      *__result = *__first;
	      ++__first;
	      ++__result;
	    }
	  return __result;
	}
    };


  template<>
    struct __copy_move<true, false, random_access_iterator_tag>
    {
      template<typename _II, typename _OI>
	_GLIBCXX20_CONSTEXPR
	static _OI
	__copy_m(_II __first, _II __last, _OI __result)
	{
	  typedef typename iterator_traits<_II>::difference_type _Distance;
	  for(_Distance __n = __last - __first; __n > 0; --__n)
	    {
	      *__result = std::move(*__first);
	      ++__first;
	      ++__result;
	    }
	  return __result;
	}
    };


  template<bool _IsMove>
    struct __copy_move<_IsMove, true, random_access_iterator_tag>
    {
      template<typename _Tp>
	_GLIBCXX20_CONSTEXPR
	static _Tp*
	__copy_m(const _Tp* __first, const _Tp* __last, _Tp* __result)
	{

	  using __assignable = conditional<_IsMove,
					   is_move_assignable<_Tp>,
					   is_copy_assignable<_Tp>>;
	  // trivial types can have deleted assignment
	  static_assert( __assignable::type::value, "type is not assignable" );

	  const ptrdiff_t _Num = __last - __first;
	  if (_Num)
	    __builtin_memmove(__result, __first, sizeof(_Tp) * _Num);
	  return __result + _Num;
	}
    };

_GLIBCXX_BEGIN_NAMESPACE_CONTAINER

  template<typename _Tp, typename _Ref, typename _Ptr>
    struct _Deque_iterator;

  struct _Bit_iterator;

_GLIBCXX_END_NAMESPACE_CONTAINER

  // Helpers for streambuf iterators (either istream or ostream).
  // NB: avoid including <iosfwd>, relatively large.
  template<typename _CharT>
    struct char_traits;

  template<typename _CharT, typename _Traits>
    class istreambuf_iterator;

  template<typename _CharT, typename _Traits>
    class ostreambuf_iterator;

  template<bool _IsMove, typename _CharT>
    typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
	     ostreambuf_iterator<_CharT, char_traits<_CharT> > >::__type
    __copy_move_a2(_CharT*, _CharT*,
		   ostreambuf_iterator<_CharT, char_traits<_CharT> >);

  template<bool _IsMove, typename _CharT>
    typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
	     ostreambuf_iterator<_CharT, char_traits<_CharT> > >::__type
    __copy_move_a2(const _CharT*, const _CharT*,
		   ostreambuf_iterator<_CharT, char_traits<_CharT> >);

  template<bool _IsMove, typename _CharT>
    typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
				    _CharT*>::__type
    __copy_move_a2(istreambuf_iterator<_CharT, char_traits<_CharT> >,
		   istreambuf_iterator<_CharT, char_traits<_CharT> >, _CharT*);

  template<bool _IsMove, typename _CharT>
    typename __gnu_cxx::__enable_if<
      __is_char<_CharT>::__value,
      _GLIBCXX_STD_C::_Deque_iterator<_CharT, _CharT&, _CharT*> >::__type
    __copy_move_a2(
	istreambuf_iterator<_CharT, char_traits<_CharT> >,
	istreambuf_iterator<_CharT, char_traits<_CharT> >,
	_GLIBCXX_STD_C::_Deque_iterator<_CharT, _CharT&, _CharT*>);

  template<bool _IsMove, typename _II, typename _OI>
    _GLIBCXX20_CONSTEXPR
    inline _OI
    __copy_move_a2(_II __first, _II __last, _OI __result)
    {
      typedef typename iterator_traits<_II>::iterator_category _Category;





      return std::__copy_move<_IsMove, __memcpyable<_OI, _II>::__value,
			      _Category>::__copy_m(__first, __last, __result);
    }

  template<bool _IsMove,
	   typename _Tp, typename _Ref, typename _Ptr, typename _OI>
    _OI
    __copy_move_a1(_GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>,
		   _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>,
		   _OI);

  template<bool _IsMove,
	   typename _ITp, typename _IRef, typename _IPtr, typename _OTp>
    _GLIBCXX_STD_C::_Deque_iterator<_OTp, _OTp&, _OTp*>
    __copy_move_a1(_GLIBCXX_STD_C::_Deque_iterator<_ITp, _IRef, _IPtr>,
		   _GLIBCXX_STD_C::_Deque_iterator<_ITp, _IRef, _IPtr>,
		   _GLIBCXX_STD_C::_Deque_iterator<_OTp, _OTp&, _OTp*>);

  template<bool _IsMove, typename _II, typename _Tp>
    typename __gnu_cxx::__enable_if<
      __is_random_access_iter<_II>::__value,
      _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Tp&, _Tp*> >::__type
    __copy_move_a1(_II, _II, _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Tp&, _Tp*>);

  template<bool _IsMove, typename _II, typename _OI>
    _GLIBCXX20_CONSTEXPR
    inline _OI
    __copy_move_a1(_II __first, _II __last, _OI __result)
    { return std::__copy_move_a2<_IsMove>(__first, __last, __result); }

  template<bool _IsMove, typename _II, typename _OI>
    _GLIBCXX20_CONSTEXPR
    inline _OI
    __copy_move_a(_II __first, _II __last, _OI __result)
    {
      return std::__niter_wrap(__result,
		std::__copy_move_a1<_IsMove>(std::__niter_base(__first),
					     std::__niter_base(__last),
					     std::__niter_base(__result)));
    }

  template<bool _IsMove,
	   typename _Ite, typename _Seq, typename _Cat, typename _OI>
    _OI
    __copy_move_a(const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&,
		  const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&,
		  _OI);

  template<bool _IsMove,
	   typename _II, typename _Ite, typename _Seq, typename _Cat>
    __gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>
    __copy_move_a(_II, _II,
		  const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&);

  template<bool _IsMove,
	   typename _IIte, typename _ISeq, typename _ICat,
	   typename _OIte, typename _OSeq, typename _OCat>
    ::__gnu_debug::_Safe_iterator<_OIte, _OSeq, _OCat>
    __copy_move_a(const ::__gnu_debug::_Safe_iterator<_IIte, _ISeq, _ICat>&,
		  const ::__gnu_debug::_Safe_iterator<_IIte, _ISeq, _ICat>&,
		  const ::__gnu_debug::_Safe_iterator<_OIte, _OSeq, _OCat>&);

  template<typename _InputIterator, typename _Size, typename _OutputIterator>
    _GLIBCXX20_CONSTEXPR
    _OutputIterator
    __copy_n_a(_InputIterator __first, _Size __n, _OutputIterator __result,
	       bool)
    {
      if (__n > 0)
	{
	  while (true)
	    {
	      *__result = *__first;
	      ++__result;
	      if (--__n > 0)
		++__first;
	      else
		break;
	    }
	}
      return __result;
    }

  template<typename _CharT, typename _Size>
    typename __gnu_cxx::__enable_if<
      __is_char<_CharT>::__value, _CharT*>::__type
    __copy_n_a(istreambuf_iterator<_CharT, char_traits<_CharT> >,
	       _Size, _CharT*, bool);

  template<typename _CharT, typename _Size>
    typename __gnu_cxx::__enable_if<
      __is_char<_CharT>::__value,
      _GLIBCXX_STD_C::_Deque_iterator<_CharT, _CharT&, _CharT*> >::__type
    __copy_n_a(istreambuf_iterator<_CharT, char_traits<_CharT> >, _Size,
	       _GLIBCXX_STD_C::_Deque_iterator<_CharT, _CharT&, _CharT*>,
	       bool);

  /**
   *  @brief Copies the range [first,last) into result.
   *  @ingroup mutating_algorithms
   *  @param  __first  An input iterator.
   *  @param  __last   An input iterator.
   *  @param  __result An output iterator.
   *  @return   result + (last - first)
   *
   *  This inline function will boil down to a call to @c memmove whenever
   *  possible.  Failing that, if random access iterators are passed, then the
   *  loop count will be known (and therefore a candidate for compiler
   *  optimizations such as unrolling).  Result may not be contained within
   *  [first,last); the copy_backward function should be used instead.
   *
   *  Note that the end of the output range is permitted to be contained
   *  within [first,last).
  */
  template<typename _II, typename _OI>
    _GLIBCXX20_CONSTEXPR
    inline _OI
    copy(_II __first, _II __last, _OI __result)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_II>)
      __glibcxx_function_requires(_OutputIteratorConcept<_OI,
	    typename iterator_traits<_II>::value_type>)
      __glibcxx_requires_can_increment_range(__first, __last, __result);

      return std::__copy_move_a<__is_move_iterator<_II>::__value>
	     (std::__miter_base(__first), std::__miter_base(__last), __result);
    }


  /**
   *  @brief Moves the range [first,last) into result.
   *  @ingroup mutating_algorithms
   *  @param  __first  An input iterator.
   *  @param  __last   An input iterator.
   *  @param  __result An output iterator.
   *  @return   result + (last - first)
   *
   *  This inline function will boil down to a call to @c memmove whenever
   *  possible.  Failing that, if random access iterators are passed, then the
   *  loop count will be known (and therefore a candidate for compiler
   *  optimizations such as unrolling).  Result may not be contained within
   *  [first,last); the move_backward function should be used instead.
   *
   *  Note that the end of the output range is permitted to be contained
   *  within [first,last).
  */
  template<typename _II, typename _OI>
    _GLIBCXX20_CONSTEXPR
    inline _OI
    move(_II __first, _II __last, _OI __result)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_II>)
      __glibcxx_function_requires(_OutputIteratorConcept<_OI,
	    typename iterator_traits<_II>::value_type>)
      __glibcxx_requires_can_increment_range(__first, __last, __result);

      return std::__copy_move_a<true>(std::__miter_base(__first),
				      std::__miter_base(__last), __result);
    }

#define _GLIBCXX_MOVE3(_Tp,_Up,_Vp) std::move(_Tp, _Up, _Vp)




  template<bool _IsMove, bool _IsSimple, typename _Category>
    struct __copy_move_backward
    {
      template<typename _BI1, typename _BI2>
	_GLIBCXX20_CONSTEXPR
	static _BI2
	__copy_move_b(_BI1 __first, _BI1 __last, _BI2 __result)
	{
	  while (__first != __last)
	    *--__result = *--__last;
	  return __result;
	}
    };


  template<typename _Category>
    struct __copy_move_backward<true, false, _Category>
    {
      template<typename _BI1, typename _BI2>
	_GLIBCXX20_CONSTEXPR
	static _BI2
	__copy_move_b(_BI1 __first, _BI1 __last, _BI2 __result)
	{
	  while (__first != __last)
	    *--__result = std::move(*--__last);
	  return __result;
	}
    };


  template<>
    struct __copy_move_backward<false, false, random_access_iterator_tag>
    {
      template<typename _BI1, typename _BI2>
	_GLIBCXX20_CONSTEXPR
	static _BI2
	__copy_move_b(_BI1 __first, _BI1 __last, _BI2 __result)
	{
	  typename iterator_traits<_BI1>::difference_type
	    __n = __last - __first;
	  for (; __n > 0; --__n)
	    *--__result = *--__last;
	  return __result;
	}
    };


  template<>
    struct __copy_move_backward<true, false, random_access_iterator_tag>
    {
      template<typename _BI1, typename _BI2>
	_GLIBCXX20_CONSTEXPR
	static _BI2
	__copy_move_b(_BI1 __first, _BI1 __last, _BI2 __result)
	{
	  typename iterator_traits<_BI1>::difference_type
	    __n = __last - __first;
	  for (; __n > 0; --__n)
	    *--__result = std::move(*--__last);
	  return __result;
	}
    };


  template<bool _IsMove>
    struct __copy_move_backward<_IsMove, true, random_access_iterator_tag>
    {
      template<typename _Tp>
	_GLIBCXX20_CONSTEXPR
	static _Tp*
	__copy_move_b(const _Tp* __first, const _Tp* __last, _Tp* __result)
	{

	  using __assignable = conditional<_IsMove,
					   is_move_assignable<_Tp>,
					   is_copy_assignable<_Tp>>;
	  // trivial types can have deleted assignment
	  static_assert( __assignable::type::value, "type is not assignable" );

	  const ptrdiff_t _Num = __last - __first;
	  if (_Num)
	    __builtin_memmove(__result - _Num, __first, sizeof(_Tp) * _Num);
	  return __result - _Num;
	}
    };

  template<bool _IsMove, typename _BI1, typename _BI2>
    _GLIBCXX20_CONSTEXPR
    inline _BI2
    __copy_move_backward_a2(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      typedef typename iterator_traits<_BI1>::iterator_category _Category;





      return std::__copy_move_backward<_IsMove,
				       __memcpyable<_BI2, _BI1>::__value,
				       _Category>::__copy_move_b(__first,
								 __last,
								 __result);
    }

  template<bool _IsMove, typename _BI1, typename _BI2>
    _GLIBCXX20_CONSTEXPR
    inline _BI2
    __copy_move_backward_a1(_BI1 __first, _BI1 __last, _BI2 __result)
    { return std::__copy_move_backward_a2<_IsMove>(__first, __last, __result); }

  template<bool _IsMove,
	   typename _Tp, typename _Ref, typename _Ptr, typename _OI>
    _OI
    __copy_move_backward_a1(_GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>,
			    _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>,
			    _OI);

  template<bool _IsMove,
	   typename _ITp, typename _IRef, typename _IPtr, typename _OTp>
    _GLIBCXX_STD_C::_Deque_iterator<_OTp, _OTp&, _OTp*>
    __copy_move_backward_a1(
			_GLIBCXX_STD_C::_Deque_iterator<_ITp, _IRef, _IPtr>,
			_GLIBCXX_STD_C::_Deque_iterator<_ITp, _IRef, _IPtr>,
			_GLIBCXX_STD_C::_Deque_iterator<_OTp, _OTp&, _OTp*>);

  template<bool _IsMove, typename _II, typename _Tp>
    typename __gnu_cxx::__enable_if<
      __is_random_access_iter<_II>::__value,
      _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Tp&, _Tp*> >::__type
    __copy_move_backward_a1(_II, _II,
			    _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Tp&, _Tp*>);

  template<bool _IsMove, typename _II, typename _OI>
    _GLIBCXX20_CONSTEXPR
    inline _OI
    __copy_move_backward_a(_II __first, _II __last, _OI __result)
    {
      return std::__niter_wrap(__result,
		std::__copy_move_backward_a1<_IsMove>
		  (std::__niter_base(__first), std::__niter_base(__last),
		   std::__niter_base(__result)));
    }

  template<bool _IsMove,
	   typename _Ite, typename _Seq, typename _Cat, typename _OI>
    _OI
    __copy_move_backward_a(
		const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&,
		const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&,
		_OI);

  template<bool _IsMove,
	   typename _II, typename _Ite, typename _Seq, typename _Cat>
    __gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>
    __copy_move_backward_a(_II, _II,
		const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&);

  template<bool _IsMove,
	   typename _IIte, typename _ISeq, typename _ICat,
	   typename _OIte, typename _OSeq, typename _OCat>
    ::__gnu_debug::_Safe_iterator<_OIte, _OSeq, _OCat>
    __copy_move_backward_a(
		const ::__gnu_debug::_Safe_iterator<_IIte, _ISeq, _ICat>&,
		const ::__gnu_debug::_Safe_iterator<_IIte, _ISeq, _ICat>&,
		const ::__gnu_debug::_Safe_iterator<_OIte, _OSeq, _OCat>&);

  /**
   *  @brief Copies the range [first,last) into result.
   *  @ingroup mutating_algorithms
   *  @param  __first  A bidirectional iterator.
   *  @param  __last   A bidirectional iterator.
   *  @param  __result A bidirectional iterator.
   *  @return   result - (last - first)
   *
   *  The function has the same effect as copy, but starts at the end of the
   *  range and works its way to the start, returning the start of the result.
   *  This inline function will boil down to a call to @c memmove whenever
   *  possible.  Failing that, if random access iterators are passed, then the
   *  loop count will be known (and therefore a candidate for compiler
   *  optimizations such as unrolling).
   *
   *  Result may not be in the range (first,last].  Use copy instead.  Note
   *  that the start of the output range may overlap [first,last).
  */
  template<typename _BI1, typename _BI2>
    _GLIBCXX20_CONSTEXPR
    inline _BI2
    copy_backward(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      // concept requirements
      __glibcxx_function_requires(_BidirectionalIteratorConcept<_BI1>)
      __glibcxx_function_requires(_Mutable_BidirectionalIteratorConcept<_BI2>)
      __glibcxx_function_requires(_ConvertibleConcept<
	    typename iterator_traits<_BI1>::value_type,
	    typename iterator_traits<_BI2>::value_type>)
      __glibcxx_requires_can_decrement_range(__first, __last, __result);

      return std::__copy_move_backward_a<__is_move_iterator<_BI1>::__value>
	     (std::__miter_base(__first), std::__miter_base(__last), __result);
    }


  /**
   *  @brief Moves the range [first,last) into result.
   *  @ingroup mutating_algorithms
   *  @param  __first  A bidirectional iterator.
   *  @param  __last   A bidirectional iterator.
   *  @param  __result A bidirectional iterator.
   *  @return   result - (last - first)
   *
   *  The function has the same effect as move, but starts at the end of the
   *  range and works its way to the start, returning the start of the result.
   *  This inline function will boil down to a call to @c memmove whenever
   *  possible.  Failing that, if random access iterators are passed, then the
   *  loop count will be known (and therefore a candidate for compiler
   *  optimizations such as unrolling).
   *
   *  Result may not be in the range (first,last].  Use move instead.  Note
   *  that the start of the output range may overlap [first,last).
  */
  template<typename _BI1, typename _BI2>
    _GLIBCXX20_CONSTEXPR
    inline _BI2
    move_backward(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      // concept requirements
      __glibcxx_function_requires(_BidirectionalIteratorConcept<_BI1>)
      __glibcxx_function_requires(_Mutable_BidirectionalIteratorConcept<_BI2>)
      __glibcxx_function_requires(_ConvertibleConcept<
	    typename iterator_traits<_BI1>::value_type,
	    typename iterator_traits<_BI2>::value_type>)
      __glibcxx_requires_can_decrement_range(__first, __last, __result);

      return std::__copy_move_backward_a<true>(std::__miter_base(__first),
					       std::__miter_base(__last),
					       __result);
    }

#define _GLIBCXX_MOVE_BACKWARD3(_Tp,_Up,_Vp) std::move_backward(_Tp, _Up, _Vp)




  template<typename _ForwardIterator, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline typename
    __gnu_cxx::__enable_if<!__is_scalar<_Tp>::__value, void>::__type
    __fill_a1(_ForwardIterator __first, _ForwardIterator __last,
	      const _Tp& __value)
    {
      for (; __first != __last; ++__first)
	*__first = __value;
    }

  template<typename _ForwardIterator, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline typename
    __gnu_cxx::__enable_if<__is_scalar<_Tp>::__value, void>::__type
    __fill_a1(_ForwardIterator __first, _ForwardIterator __last,
	      const _Tp& __value)
    {
      const _Tp __tmp = __value;
      for (; __first != __last; ++__first)
	*__first = __tmp;
    }

  // Specialization: for char types we can use memset.
  template<typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline typename
    __gnu_cxx::__enable_if<__is_byte<_Tp>::__value, void>::__type
    __fill_a1(_Tp* __first, _Tp* __last, const _Tp& __c)
    {
      const _Tp __tmp = __c;
      if (const size_t __len = __last - __first)
	__builtin_memset(__first, static_cast<unsigned char>(__tmp), __len);
    }

  template<typename _Ite, typename _Cont, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline void
    __fill_a1(::__gnu_cxx::__normal_iterator<_Ite, _Cont> __first,
	      ::__gnu_cxx::__normal_iterator<_Ite, _Cont> __last,
	      const _Tp& __value)
    { std::__fill_a1(__first.base(), __last.base(), __value); }

  template<typename _Tp, typename _VTp>
    void
    __fill_a1(const _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Tp&, _Tp*>&,
	      const _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Tp&, _Tp*>&,
	      const _VTp&);

  void
  __fill_a1(_GLIBCXX_STD_C::_Bit_iterator, _GLIBCXX_STD_C::_Bit_iterator,
	    const bool&);

  template<typename _FIte, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline void
    __fill_a(_FIte __first, _FIte __last, const _Tp& __value)
    { std::__fill_a1(__first, __last, __value); }

  template<typename _Ite, typename _Seq, typename _Cat, typename _Tp>
    void
    __fill_a(const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&,
	     const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&,
	     const _Tp&);

  /**
   *  @brief Fills the range [first,last) with copies of value.
   *  @ingroup mutating_algorithms
   *  @param  __first  A forward iterator.
   *  @param  __last   A forward iterator.
   *  @param  __value  A reference-to-const of arbitrary type.
   *  @return   Nothing.
   *
   *  This function fills a range with copies of the same value.  For char
   *  types filling contiguous areas of memory, this becomes an inline call
   *  to @c memset or @c wmemset.
  */
  template<typename _ForwardIterator, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline void
    fill(_ForwardIterator __first, _ForwardIterator __last, const _Tp& __value)
    {
      // concept requirements
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator>)
      __glibcxx_requires_valid_range(__first, __last);

      std::__fill_a(__first, __last, __value);
    }

  // Used by fill_n, generate_n, etc. to convert _Size to an integral type:
  inline _GLIBCXX_CONSTEXPR int
  __size_to_integer(int __n) { return __n; }
  inline _GLIBCXX_CONSTEXPR unsigned
  __size_to_integer(unsigned __n) { return __n; }
  inline _GLIBCXX_CONSTEXPR long
  __size_to_integer(long __n) { return __n; }
  inline _GLIBCXX_CONSTEXPR unsigned long
  __size_to_integer(unsigned long __n) { return __n; }
  inline _GLIBCXX_CONSTEXPR long long
  __size_to_integer(long long __n) { return __n; }
  inline _GLIBCXX_CONSTEXPR unsigned long long
  __size_to_integer(unsigned long long __n) { return __n; }


























  inline _GLIBCXX_CONSTEXPR long long
  __size_to_integer(float __n) { return (long long)__n; }
  inline _GLIBCXX_CONSTEXPR long long
  __size_to_integer(double __n) { return (long long)__n; }
  inline _GLIBCXX_CONSTEXPR long long
  __size_to_integer(long double __n) { return (long long)__n; }





  template<typename _OutputIterator, typename _Size, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline typename
    __gnu_cxx::__enable_if<!__is_scalar<_Tp>::__value, _OutputIterator>::__type
    __fill_n_a1(_OutputIterator __first, _Size __n, const _Tp& __value)
    {
      for (; __n > 0; --__n, (void) ++__first)
	*__first = __value;
      return __first;
    }

  template<typename _OutputIterator, typename _Size, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline typename
    __gnu_cxx::__enable_if<__is_scalar<_Tp>::__value, _OutputIterator>::__type
    __fill_n_a1(_OutputIterator __first, _Size __n, const _Tp& __value)
    {
      const _Tp __tmp = __value;
      for (; __n > 0; --__n, (void) ++__first)
	*__first = __tmp;
      return __first;
    }

  template<typename _Ite, typename _Seq, typename _Cat, typename _Size,
	   typename _Tp>
    ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>
    __fill_n_a(const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>& __first,
	       _Size __n, const _Tp& __value,
	       std::input_iterator_tag);

  template<typename _OutputIterator, typename _Size, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline _OutputIterator
    __fill_n_a(_OutputIterator __first, _Size __n, const _Tp& __value,
	       std::output_iterator_tag)
    {

      static_assert(is_integral<_Size>{}, "fill_n must pass integral size");

      return __fill_n_a1(__first, __n, __value);
    }

  template<typename _OutputIterator, typename _Size, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline _OutputIterator
    __fill_n_a(_OutputIterator __first, _Size __n, const _Tp& __value,
	       std::input_iterator_tag)
    {

      static_assert(is_integral<_Size>{}, "fill_n must pass integral size");

      return __fill_n_a1(__first, __n, __value);
    }

  template<typename _OutputIterator, typename _Size, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline _OutputIterator
    __fill_n_a(_OutputIterator __first, _Size __n, const _Tp& __value,
	       std::random_access_iterator_tag)
    {

      static_assert(is_integral<_Size>{}, "fill_n must pass integral size");

      if (__n <= 0)
	return __first;

      __glibcxx_requires_can_increment(__first, __n);

      std::__fill_a(__first, __first + __n, __value);
      return __first + __n;
    }

  /**
   *  @brief Fills the range [first,first+n) with copies of value.
   *  @ingroup mutating_algorithms
   *  @param  __first  An output iterator.
   *  @param  __n      The count of copies to perform.
   *  @param  __value  A reference-to-const of arbitrary type.
   *  @return   The iterator at first+n.
   *
   *  This function fills a range with copies of the same value.  For char
   *  types filling contiguous areas of memory, this becomes an inline call
   *  to @c memset or @c wmemset.
   *
   *  If @p __n is negative, the function does nothing.
  */
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // DR 865. More algorithms that throw away information
  // DR 426. search_n(), fill_n(), and generate_n() with negative n
  template<typename _OI, typename _Size, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline _OI
    fill_n(_OI __first, _Size __n, const _Tp& __value)
    {
      // concept requirements
      __glibcxx_function_requires(_OutputIteratorConcept<_OI, _Tp>)

      return std::__fill_n_a(__first, std::__size_to_integer(__n), __value,
			       std::__iterator_category(__first));
    }

  template<bool _BoolType>
    struct __equal
    {
      template<typename _II1, typename _II2>
	_GLIBCXX20_CONSTEXPR
	static bool
	equal(_II1 __first1, _II1 __last1, _II2 __first2)
	{
	  for (; __first1 != __last1; ++__first1, (void) ++__first2)
	    if (!(*__first1 == *__first2))
	      return false;
	  return true;
	}
    };

  template<>
    struct __equal<true>
    {
      template<typename _Tp>
	_GLIBCXX20_CONSTEXPR
	static bool
	equal(const _Tp* __first1, const _Tp* __last1, const _Tp* __first2)
	{
	  if (const size_t __len = (__last1 - __first1))
	    return !std::__memcmp(__first1, __first2, __len);
	  return true;
	}
    };

  template<typename _Tp, typename _Ref, typename _Ptr, typename _II>
    typename __gnu_cxx::__enable_if<
      __is_random_access_iter<_II>::__value, bool>::__type
    __equal_aux1(_GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>,
		 _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>,
		 _II);

  template<typename _Tp1, typename _Ref1, typename _Ptr1,
	   typename _Tp2, typename _Ref2, typename _Ptr2>
    bool
    __equal_aux1(_GLIBCXX_STD_C::_Deque_iterator<_Tp1, _Ref1, _Ptr1>,
		 _GLIBCXX_STD_C::_Deque_iterator<_Tp1, _Ref1, _Ptr1>,
		 _GLIBCXX_STD_C::_Deque_iterator<_Tp2, _Ref2, _Ptr2>);

  template<typename _II, typename _Tp, typename _Ref, typename _Ptr>
    typename __gnu_cxx::__enable_if<
      __is_random_access_iter<_II>::__value, bool>::__type
    __equal_aux1(_II, _II,
		_GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>);

  template<typename _II1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    __equal_aux1(_II1 __first1, _II1 __last1, _II2 __first2)
    {
      typedef typename iterator_traits<_II1>::value_type _ValueType1;
      const bool __simple = ((__is_integer<_ValueType1>::__value
			      || __is_pointer<_ValueType1>::__value)
			     && __memcmpable<_II1, _II2>::__value);
      return std::__equal<__simple>::equal(__first1, __last1, __first2);
    }

  template<typename _II1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    __equal_aux(_II1 __first1, _II1 __last1, _II2 __first2)
    {
      return std::__equal_aux1(std::__niter_base(__first1),
			       std::__niter_base(__last1),
			       std::__niter_base(__first2));
    }

  template<typename _II1, typename _Seq1, typename _Cat1, typename _II2>
    bool
    __equal_aux(const ::__gnu_debug::_Safe_iterator<_II1, _Seq1, _Cat1>&,
		const ::__gnu_debug::_Safe_iterator<_II1, _Seq1, _Cat1>&,
		_II2);

  template<typename _II1, typename _II2, typename _Seq2, typename _Cat2>
    bool
    __equal_aux(_II1, _II1,
		const ::__gnu_debug::_Safe_iterator<_II2, _Seq2, _Cat2>&);

  template<typename _II1, typename _Seq1, typename _Cat1,
	   typename _II2, typename _Seq2, typename _Cat2>
    bool
    __equal_aux(const ::__gnu_debug::_Safe_iterator<_II1, _Seq1, _Cat1>&,
		const ::__gnu_debug::_Safe_iterator<_II1, _Seq1, _Cat1>&,
		const ::__gnu_debug::_Safe_iterator<_II2, _Seq2, _Cat2>&);

  template<typename, typename>
    struct __lc_rai
    {
      template<typename _II1, typename _II2>
	_GLIBCXX20_CONSTEXPR
	static _II1
	__newlast1(_II1, _II1 __last1, _II2, _II2)
	{ return __last1; }

      template<typename _II>
	_GLIBCXX20_CONSTEXPR
	static bool
	__cnd2(_II __first, _II __last)
	{ return __first != __last; }
    };

  template<>
    struct __lc_rai<random_access_iterator_tag, random_access_iterator_tag>
    {
      template<typename _RAI1, typename _RAI2>
	_GLIBCXX20_CONSTEXPR
	static _RAI1
	__newlast1(_RAI1 __first1, _RAI1 __last1,
		   _RAI2 __first2, _RAI2 __last2)
	{
	  const typename iterator_traits<_RAI1>::difference_type
	    __diff1 = __last1 - __first1;
	  const typename iterator_traits<_RAI2>::difference_type
	    __diff2 = __last2 - __first2;
	  return __diff2 < __diff1 ? __first1 + __diff2 : __last1;
	}

      template<typename _RAI>
	static _GLIBCXX20_CONSTEXPR bool
	__cnd2(_RAI, _RAI)
	{ return true; }
    };

  template<typename _II1, typename _II2, typename _Compare>
    _GLIBCXX20_CONSTEXPR
    bool
    __lexicographical_compare_impl(_II1 __first1, _II1 __last1,
				   _II2 __first2, _II2 __last2,
				   _Compare __comp)
    {
      typedef typename iterator_traits<_II1>::iterator_category _Category1;
      typedef typename iterator_traits<_II2>::iterator_category _Category2;
      typedef std::__lc_rai<_Category1, _Category2> __rai_type;

      __last1 = __rai_type::__newlast1(__first1, __last1, __first2, __last2);
      for (; __first1 != __last1 && __rai_type::__cnd2(__first2, __last2);
	   ++__first1, (void)++__first2)
	{
	  if (__comp(__first1, __first2))
	    return true;
	  if (__comp(__first2, __first1))
	    return false;
	}
      return __first1 == __last1 && __first2 != __last2;
    }

  template<bool _BoolType>
    struct __lexicographical_compare
    {
      template<typename _II1, typename _II2>
	_GLIBCXX20_CONSTEXPR
	static bool
	__lc(_II1 __first1, _II1 __last1, _II2 __first2, _II2 __last2)
	{
	  using __gnu_cxx::__ops::__iter_less_iter;
	  return std::__lexicographical_compare_impl(__first1, __last1,
						     __first2, __last2,
						     __iter_less_iter());
	}

      template<typename _II1, typename _II2>
	_GLIBCXX20_CONSTEXPR
	static int
	__3way(_II1 __first1, _II1 __last1, _II2 __first2, _II2 __last2)
	{
	  while (__first1 != __last1)
	    {
	      if (__first2 == __last2)
		return +1;
	      if (*__first1 < *__first2)
		return -1;
	      if (*__first2 < *__first1)
		return +1;
	      ++__first1;
	      ++__first2;
	    }
	  return int(__first2 == __last2) - 1;
	}
    };

  template<>
    struct __lexicographical_compare<true>
    {
      template<typename _Tp, typename _Up>
	_GLIBCXX20_CONSTEXPR
	static bool
	__lc(const _Tp* __first1, const _Tp* __last1,
	     const _Up* __first2, const _Up* __last2)
	{ return __3way(__first1, __last1, __first2, __last2) < 0; }

      template<typename _Tp, typename _Up>
	_GLIBCXX20_CONSTEXPR
	static ptrdiff_t
	__3way(const _Tp* __first1, const _Tp* __last1,
	       const _Up* __first2, const _Up* __last2)
	{
	  const size_t __len1 = __last1 - __first1;
	  const size_t __len2 = __last2 - __first2;
	  if (const size_t __len = std::min(__len1, __len2))
	    if (int __result = std::__memcmp(__first1, __first2, __len))
	      return __result;
	  return ptrdiff_t(__len1 - __len2);
	}
    };

  template<typename _II1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    __lexicographical_compare_aux1(_II1 __first1, _II1 __last1,
				   _II2 __first2, _II2 __last2)
    {
      typedef typename iterator_traits<_II1>::value_type _ValueType1;
      typedef typename iterator_traits<_II2>::value_type _ValueType2;
      const bool __simple =
	(__is_memcmp_ordered_with<_ValueType1, _ValueType2>::__value
	 && __is_pointer<_II1>::__value
	 && __is_pointer<_II2>::__value







	 );

      return std::__lexicographical_compare<__simple>::__lc(__first1, __last1,
							    __first2, __last2);
    }

  template<typename _Tp1, typename _Ref1, typename _Ptr1,
	   typename _Tp2>
    bool
    __lexicographical_compare_aux1(
	_GLIBCXX_STD_C::_Deque_iterator<_Tp1, _Ref1, _Ptr1>,
	_GLIBCXX_STD_C::_Deque_iterator<_Tp1, _Ref1, _Ptr1>,
	_Tp2*, _Tp2*);

  template<typename _Tp1,
	   typename _Tp2, typename _Ref2, typename _Ptr2>
    bool
    __lexicographical_compare_aux1(_Tp1*, _Tp1*,
	_GLIBCXX_STD_C::_Deque_iterator<_Tp2, _Ref2, _Ptr2>,
	_GLIBCXX_STD_C::_Deque_iterator<_Tp2, _Ref2, _Ptr2>);

  template<typename _Tp1, typename _Ref1, typename _Ptr1,
	   typename _Tp2, typename _Ref2, typename _Ptr2>
    bool
    __lexicographical_compare_aux1(
	_GLIBCXX_STD_C::_Deque_iterator<_Tp1, _Ref1, _Ptr1>,
	_GLIBCXX_STD_C::_Deque_iterator<_Tp1, _Ref1, _Ptr1>,
	_GLIBCXX_STD_C::_Deque_iterator<_Tp2, _Ref2, _Ptr2>,
	_GLIBCXX_STD_C::_Deque_iterator<_Tp2, _Ref2, _Ptr2>);

  template<typename _II1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    __lexicographical_compare_aux(_II1 __first1, _II1 __last1,
				  _II2 __first2, _II2 __last2)
    {
      return std::__lexicographical_compare_aux1(std::__niter_base(__first1),
						 std::__niter_base(__last1),
						 std::__niter_base(__first2),
						 std::__niter_base(__last2));
    }

  template<typename _Iter1, typename _Seq1, typename _Cat1,
	   typename _II2>
    bool
    __lexicographical_compare_aux(
		const ::__gnu_debug::_Safe_iterator<_Iter1, _Seq1, _Cat1>&,
		const ::__gnu_debug::_Safe_iterator<_Iter1, _Seq1, _Cat1>&,
		_II2, _II2);

  template<typename _II1,
	   typename _Iter2, typename _Seq2, typename _Cat2>
    bool
    __lexicographical_compare_aux(
		_II1, _II1,
		const ::__gnu_debug::_Safe_iterator<_Iter2, _Seq2, _Cat2>&,
		const ::__gnu_debug::_Safe_iterator<_Iter2, _Seq2, _Cat2>&);

  template<typename _Iter1, typename _Seq1, typename _Cat1,
	   typename _Iter2, typename _Seq2, typename _Cat2>
    bool
    __lexicographical_compare_aux(
		const ::__gnu_debug::_Safe_iterator<_Iter1, _Seq1, _Cat1>&,
		const ::__gnu_debug::_Safe_iterator<_Iter1, _Seq1, _Cat1>&,
		const ::__gnu_debug::_Safe_iterator<_Iter2, _Seq2, _Cat2>&,
		const ::__gnu_debug::_Safe_iterator<_Iter2, _Seq2, _Cat2>&);

  template<typename _ForwardIterator, typename _Tp, typename _Compare>
    _GLIBCXX20_CONSTEXPR
    _ForwardIterator
    __lower_bound(_ForwardIterator __first, _ForwardIterator __last,
		  const _Tp& __val, _Compare __comp)
    {
      typedef typename iterator_traits<_ForwardIterator>::difference_type
	_DistanceType;

      _DistanceType __len = std::distance(__first, __last);

      while (__len > 0)
	{
	  _DistanceType __half = __len >> 1;
	  _ForwardIterator __middle = __first;
	  std::advance(__middle, __half);
	  if (__comp(__middle, __val))
	    {
	      __first = __middle;
	      ++__first;
	      __len = __len - __half - 1;
	    }
	  else
	    __len = __half;
	}
      return __first;
    }

  /**
   *  @brief Finds the first position in which @a val could be inserted
   *         without changing the ordering.
   *  @param  __first   An iterator.
   *  @param  __last    Another iterator.
   *  @param  __val     The search term.
   *  @return         An iterator pointing to the first element <em>not less
   *                  than</em> @a val, or end() if every element is less than
   *                  @a val.
   *  @ingroup binary_search_algorithms
  */
  template<typename _ForwardIterator, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline _ForwardIterator
    lower_bound(_ForwardIterator __first, _ForwardIterator __last,
		const _Tp& __val)
    {
      // concept requirements
      __glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
      __glibcxx_function_requires(_LessThanOpConcept<
	    typename iterator_traits<_ForwardIterator>::value_type, _Tp>)
      __glibcxx_requires_partitioned_lower(__first, __last, __val);

      return std::__lower_bound(__first, __last, __val,
				__gnu_cxx::__ops::__iter_less_val());
    }

  /// This is a helper function for the sort routines and for random.tcc.
  //  Precondition: __n > 0.
  inline _GLIBCXX_CONSTEXPR int
  __lg(int __n)
  { return (int)sizeof(int) * __CHAR_BIT__  - 1 - __builtin_clz(__n); }

  inline _GLIBCXX_CONSTEXPR unsigned
  __lg(unsigned __n)
  { return (int)sizeof(int) * __CHAR_BIT__  - 1 - __builtin_clz(__n); }

  inline _GLIBCXX_CONSTEXPR long
  __lg(long __n)
  { return (int)sizeof(long) * __CHAR_BIT__ - 1 - __builtin_clzl(__n); }

  inline _GLIBCXX_CONSTEXPR unsigned long
  __lg(unsigned long __n)
  { return (int)sizeof(long) * __CHAR_BIT__ - 1 - __builtin_clzl(__n); }

  inline _GLIBCXX_CONSTEXPR long long
  __lg(long long __n)
  { return (int)sizeof(long long) * __CHAR_BIT__ - 1 - __builtin_clzll(__n); }

  inline _GLIBCXX_CONSTEXPR unsigned long long
  __lg(unsigned long long __n)
  { return (int)sizeof(long long) * __CHAR_BIT__ - 1 - __builtin_clzll(__n); }

_GLIBCXX_BEGIN_NAMESPACE_ALGO

  /**
   *  @brief Tests a range for element-wise equality.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @return   A boolean true or false.
   *
   *  This compares the elements of two ranges using @c == and returns true or
   *  false depending on whether all of the corresponding elements of the
   *  ranges are equal.
  */
  template<typename _II1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    equal(_II1 __first1, _II1 __last1, _II2 __first2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_II1>)
      __glibcxx_function_requires(_InputIteratorConcept<_II2>)
      __glibcxx_function_requires(_EqualOpConcept<
	    typename iterator_traits<_II1>::value_type,
	    typename iterator_traits<_II2>::value_type>)
      __glibcxx_requires_can_increment_range(__first1, __last1, __first2);

      return std::__equal_aux(__first1, __last1, __first2);
    }

  /**
   *  @brief Tests a range for element-wise equality.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param __binary_pred A binary predicate @link functors
   *                  functor@endlink.
   *  @return         A boolean true or false.
   *
   *  This compares the elements of two ranges using the binary_pred
   *  parameter, and returns true or
   *  false depending on whether all of the corresponding elements of the
   *  ranges are equal.
  */
  template<typename _IIter1, typename _IIter2, typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    inline bool
    equal(_IIter1 __first1, _IIter1 __last1,
	  _IIter2 __first2, _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_IIter1>)
      __glibcxx_function_requires(_InputIteratorConcept<_IIter2>)
      __glibcxx_requires_valid_range(__first1, __last1);

      for (; __first1 != __last1; ++__first1, (void)++__first2)
	if (!bool(__binary_pred(*__first1, *__first2)))
	  return false;
      return true;
    }


  // 4-iterator version of std::equal<It1, It2> for use in C++11.
  template<typename _II1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    __equal4(_II1 __first1, _II1 __last1, _II2 __first2, _II2 __last2)
    {
      using _RATag = random_access_iterator_tag;
      using _Cat1 = typename iterator_traits<_II1>::iterator_category;
      using _Cat2 = typename iterator_traits<_II2>::iterator_category;
      using _RAIters = __and_<is_same<_Cat1, _RATag>, is_same<_Cat2, _RATag>>;
      if (_RAIters())
	{
	  auto __d1 = std::distance(__first1, __last1);
	  auto __d2 = std::distance(__first2, __last2);
	  if (__d1 != __d2)
	    return false;
	  return _GLIBCXX_STD_A::equal(__first1, __last1, __first2);
	}

      for (; __first1 != __last1 && __first2 != __last2;
	  ++__first1, (void)++__first2)
	if (!(*__first1 == *__first2))
	  return false;
      return __first1 == __last1 && __first2 == __last2;
    }

  // 4-iterator version of std::equal<It1, It2, BinaryPred> for use in C++11.
  template<typename _II1, typename _II2, typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    inline bool
    __equal4(_II1 __first1, _II1 __last1, _II2 __first2, _II2 __last2,
	     _BinaryPredicate __binary_pred)
    {
      using _RATag = random_access_iterator_tag;
      using _Cat1 = typename iterator_traits<_II1>::iterator_category;
      using _Cat2 = typename iterator_traits<_II2>::iterator_category;
      using _RAIters = __and_<is_same<_Cat1, _RATag>, is_same<_Cat2, _RATag>>;
      if (_RAIters())
	{
	  auto __d1 = std::distance(__first1, __last1);
	  auto __d2 = std::distance(__first2, __last2);
	  if (__d1 != __d2)
	    return false;
	  return _GLIBCXX_STD_A::equal(__first1, __last1, __first2,
				       __binary_pred);
	}

      for (; __first1 != __last1 && __first2 != __last2;
	  ++__first1, (void)++__first2)
	if (!bool(__binary_pred(*__first1, *__first2)))
	  return false;
      return __first1 == __last1 && __first2 == __last2;
    }




#define __cpp_lib_robust_nonmodifying_seq_ops 201304

  /**
   *  @brief Tests a range for element-wise equality.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @return   A boolean true or false.
   *
   *  This compares the elements of two ranges using @c == and returns true or
   *  false depending on whether all of the corresponding elements of the
   *  ranges are equal.
  */
  template<typename _II1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    equal(_II1 __first1, _II1 __last1, _II2 __first2, _II2 __last2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_II1>)
      __glibcxx_function_requires(_InputIteratorConcept<_II2>)
      __glibcxx_function_requires(_EqualOpConcept<
	    typename iterator_traits<_II1>::value_type,
	    typename iterator_traits<_II2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return _GLIBCXX_STD_A::__equal4(__first1, __last1, __first2, __last2);
    }

  /**
   *  @brief Tests a range for element-wise equality.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @param __binary_pred A binary predicate @link functors
   *                  functor@endlink.
   *  @return         A boolean true or false.
   *
   *  This compares the elements of two ranges using the binary_pred
   *  parameter, and returns true or
   *  false depending on whether all of the corresponding elements of the
   *  ranges are equal.
  */
  template<typename _IIter1, typename _IIter2, typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    inline bool
    equal(_IIter1 __first1, _IIter1 __last1,
	  _IIter2 __first2, _IIter2 __last2, _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_IIter1>)
      __glibcxx_function_requires(_InputIteratorConcept<_IIter2>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return _GLIBCXX_STD_A::__equal4(__first1, __last1, __first2, __last2,
				      __binary_pred);
    }


  /**
   *  @brief Performs @b dictionary comparison on ranges.
   *  @ingroup sorting_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @return   A boolean true or false.
   *
   *  <em>Returns true if the sequence of elements defined by the range
   *  [first1,last1) is lexicographically less than the sequence of elements
   *  defined by the range [first2,last2).  Returns false otherwise.</em>
   *  (Quoted from [25.3.8]/1.)  If the iterators are all character pointers,
   *  then this is an inline call to @c memcmp.
  */
  template<typename _II1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    lexicographical_compare(_II1 __first1, _II1 __last1,
			    _II2 __first2, _II2 __last2)
    {





      __glibcxx_function_requires(_InputIteratorConcept<_II1>)
      __glibcxx_function_requires(_InputIteratorConcept<_II2>)
      __glibcxx_function_requires(_LessThanOpConcept<_ValueType1, _ValueType2>)
      __glibcxx_function_requires(_LessThanOpConcept<_ValueType2, _ValueType1>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return std::__lexicographical_compare_aux(__first1, __last1,
						__first2, __last2);
    }

  /**
   *  @brief Performs @b dictionary comparison on ranges.
   *  @ingroup sorting_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @param  __comp  A @link comparison_functors comparison functor@endlink.
   *  @return   A boolean true or false.
   *
   *  The same as the four-parameter @c lexicographical_compare, but uses the
   *  comp parameter instead of @c <.
  */
  template<typename _II1, typename _II2, typename _Compare>
    _GLIBCXX20_CONSTEXPR
    inline bool
    lexicographical_compare(_II1 __first1, _II1 __last1,
			    _II2 __first2, _II2 __last2, _Compare __comp)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_II1>)
      __glibcxx_function_requires(_InputIteratorConcept<_II2>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return std::__lexicographical_compare_impl
	(__first1, __last1, __first2, __last2,
	 __gnu_cxx::__ops::__iter_comp_iter(__comp));
    }


  template<typename _InputIterator1, typename _InputIterator2,
	   typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    pair<_InputIterator1, _InputIterator2>
    __mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	       _InputIterator2 __first2, _BinaryPredicate __binary_pred)
    {
      while (__first1 != __last1 && __binary_pred(__first1, __first2))
	{
	  ++__first1;
	  ++__first2;
	}
      return pair<_InputIterator1, _InputIterator2>(__first1, __first2);
    }

  /**
   *  @brief Finds the places in ranges which don't match.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @return   A pair of iterators pointing to the first mismatch.
   *
   *  This compares the elements of two ranges using @c == and returns a pair
   *  of iterators.  The first iterator points into the first range, the
   *  second iterator points into the second range, and the elements pointed
   *  to by the iterators are not equal.
  */
  template<typename _InputIterator1, typename _InputIterator2>
    _GLIBCXX20_CONSTEXPR
    inline pair<_InputIterator1, _InputIterator2>
    mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	     _InputIterator2 __first2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_function_requires(_EqualOpConcept<
	    typename iterator_traits<_InputIterator1>::value_type,
	    typename iterator_traits<_InputIterator2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);

      return _GLIBCXX_STD_A::__mismatch(__first1, __last1, __first2,
			     __gnu_cxx::__ops::__iter_equal_to_iter());
    }

  /**
   *  @brief Finds the places in ranges which don't match.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param __binary_pred A binary predicate @link functors
   *         functor@endlink.
   *  @return   A pair of iterators pointing to the first mismatch.
   *
   *  This compares the elements of two ranges using the binary_pred
   *  parameter, and returns a pair
   *  of iterators.  The first iterator points into the first range, the
   *  second iterator points into the second range, and the elements pointed
   *  to by the iterators are not equal.
  */
  template<typename _InputIterator1, typename _InputIterator2,
	   typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    inline pair<_InputIterator1, _InputIterator2>
    mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	     _InputIterator2 __first2, _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_requires_valid_range(__first1, __last1);

      return _GLIBCXX_STD_A::__mismatch(__first1, __last1, __first2,
	__gnu_cxx::__ops::__iter_comp_iter(__binary_pred));
    }



  template<typename _InputIterator1, typename _InputIterator2,
	   typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    pair<_InputIterator1, _InputIterator2>
    __mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	       _InputIterator2 __first2, _InputIterator2 __last2,
	       _BinaryPredicate __binary_pred)
    {
      while (__first1 != __last1 && __first2 != __last2
	     && __binary_pred(__first1, __first2))
	{
	  ++__first1;
	  ++__first2;
	}
      return pair<_InputIterator1, _InputIterator2>(__first1, __first2);
    }

  /**
   *  @brief Finds the places in ranges which don't match.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @return   A pair of iterators pointing to the first mismatch.
   *
   *  This compares the elements of two ranges using @c == and returns a pair
   *  of iterators.  The first iterator points into the first range, the
   *  second iterator points into the second range, and the elements pointed
   *  to by the iterators are not equal.
  */
  template<typename _InputIterator1, typename _InputIterator2>
    _GLIBCXX20_CONSTEXPR
    inline pair<_InputIterator1, _InputIterator2>
    mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	     _InputIterator2 __first2, _InputIterator2 __last2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_function_requires(_EqualOpConcept<
	    typename iterator_traits<_InputIterator1>::value_type,
	    typename iterator_traits<_InputIterator2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return _GLIBCXX_STD_A::__mismatch(__first1, __last1, __first2, __last2,
			     __gnu_cxx::__ops::__iter_equal_to_iter());
    }

  /**
   *  @brief Finds the places in ranges which don't match.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @param __binary_pred A binary predicate @link functors
   *         functor@endlink.
   *  @return   A pair of iterators pointing to the first mismatch.
   *
   *  This compares the elements of two ranges using the binary_pred
   *  parameter, and returns a pair
   *  of iterators.  The first iterator points into the first range, the
   *  second iterator points into the second range, and the elements pointed
   *  to by the iterators are not equal.
  */
  template<typename _InputIterator1, typename _InputIterator2,
	   typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    inline pair<_InputIterator1, _InputIterator2>
    mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	     _InputIterator2 __first2, _InputIterator2 __last2,
	     _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return _GLIBCXX_STD_A::__mismatch(__first1, __last1, __first2, __last2,
			     __gnu_cxx::__ops::__iter_comp_iter(__binary_pred));
    }


_GLIBCXX_END_NAMESPACE_ALGO

  /// This is an overload used by find algos for the Input Iterator case.
  template<typename _InputIterator, typename _Predicate>
    _GLIBCXX20_CONSTEXPR
    inline _InputIterator
    __find_if(_InputIterator __first, _InputIterator __last,
	      _Predicate __pred, input_iterator_tag)
    {
      while (__first != __last && !__pred(__first))
	++__first;
      return __first;
    }

  /// This is an overload used by find algos for the RAI case.
  template<typename _RandomAccessIterator, typename _Predicate>
    _GLIBCXX20_CONSTEXPR
    _RandomAccessIterator
    __find_if(_RandomAccessIterator __first, _RandomAccessIterator __last,
	      _Predicate __pred, random_access_iterator_tag)
    {
      typename iterator_traits<_RandomAccessIterator>::difference_type
	__trip_count = (__last - __first) >> 2;

      for (; __trip_count > 0; --__trip_count)
	{
	  if (__pred(__first))
	    return __first;
	  ++__first;

	  if (__pred(__first))
	    return __first;
	  ++__first;

	  if (__pred(__first))
	    return __first;
	  ++__first;

	  if (__pred(__first))
	    return __first;
	  ++__first;
	}

      switch (__last - __first)
	{
	case 3:
	  if (__pred(__first))
	    return __first;
	  ++__first;
	  // FALLTHRU
	case 2:
	  if (__pred(__first))
	    return __first;
	  ++__first;
	  // FALLTHRU
	case 1:
	  if (__pred(__first))
	    return __first;
	  ++__first;
	  // FALLTHRU
	case 0:
	default:
	  return __last;
	}
    }

  template<typename _Iterator, typename _Predicate>
    _GLIBCXX20_CONSTEXPR
    inline _Iterator
    __find_if(_Iterator __first, _Iterator __last, _Predicate __pred)
    {
      return __find_if(__first, __last, __pred,
		       std::__iterator_category(__first));
    }

  template<typename _InputIterator, typename _Predicate>
    _GLIBCXX20_CONSTEXPR
    typename iterator_traits<_InputIterator>::difference_type
    __count_if(_InputIterator __first, _InputIterator __last, _Predicate __pred)
    {
      typename iterator_traits<_InputIterator>::difference_type __n = 0;
      for (; __first != __last; ++__first)
	if (__pred(__first))
	  ++__n;
      return __n;
    }


  template<typename _ForwardIterator1, typename _ForwardIterator2,
	   typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    bool
    __is_permutation(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
		     _ForwardIterator2 __first2, _BinaryPredicate __pred)
    {
      // Efficiently compare identical prefixes:  O(N) if sequences
      // have the same elements in the same order.
      for (; __first1 != __last1; ++__first1, (void)++__first2)
	if (!__pred(__first1, __first2))
	  break;

      if (__first1 == __last1)
	return true;

      // Establish __last2 assuming equal ranges by iterating over the
      // rest of the list.
      _ForwardIterator2 __last2 = __first2;
      std::advance(__last2, std::distance(__first1, __last1));
      for (_ForwardIterator1 __scan = __first1; __scan != __last1; ++__scan)
	{
	  if (__scan != std::__find_if(__first1, __scan,
			  __gnu_cxx::__ops::__iter_comp_iter(__pred, __scan)))
	    continue; // We've seen this one before.

	  auto __matches
	    = std::__count_if(__first2, __last2,
			__gnu_cxx::__ops::__iter_comp_iter(__pred, __scan));
	  if (0 == __matches ||
	      std::__count_if(__scan, __last1,
			__gnu_cxx::__ops::__iter_comp_iter(__pred, __scan))
	      != __matches)
	    return false;
	}
      return true;
    }

  /**
   *  @brief  Checks whether a permutation of the second sequence is equal
   *          to the first sequence.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  Start of first range.
   *  @param  __last1   End of first range.
   *  @param  __first2  Start of second range.
   *  @return true if there exists a permutation of the elements in the range
   *          [__first2, __first2 + (__last1 - __first1)), beginning with
   *          ForwardIterator2 begin, such that equal(__first1, __last1, begin)
   *          returns true; otherwise, returns false.
  */
  template<typename _ForwardIterator1, typename _ForwardIterator2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    is_permutation(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
		   _ForwardIterator2 __first2)
    {
      // concept requirements
      __glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator1>)
      __glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator2>)
      __glibcxx_function_requires(_EqualOpConcept<
		typename iterator_traits<_ForwardIterator1>::value_type,
		typename iterator_traits<_ForwardIterator2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);

      return std::__is_permutation(__first1, __last1, __first2,
				   __gnu_cxx::__ops::__iter_equal_to_iter());
    }


_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

// NB: This file is included within many other C++ includes, as a way
// of getting the base algorithms. So, make sure that parallel bits
// come in too if requested.





// -*- C++ -*- forwarding header.

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/cwchar
 *  This is a Standard C++ Library file.  You should @c \#include this file
 *  in your programs, rather than any of the @a *.h implementation files.
 *
 *  This is the C++ version of the Standard C Library header @c wchar.h,
 *  and its contents are (mostly) the same as that header, but are all
 *  contained in the namespace @c std (except for names which are defined
 *  as macros in C).
 */

//
// ISO C++ 14882: 21.4
//

       












#define _GLIBCXX_ALWAYS_INLINE inline __attribute__((__always_inline__))


namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @brief  Mapping from character type to associated types.
   *
   *  @note This is an implementation class for the generic version
   *  of char_traits.  It defines int_type, off_type, pos_type, and
   *  state_type.  By default these are unsigned long, streamoff,
   *  streampos, and mbstate_t.  Users who need a different set of
   *  types, but who don't need to change the definitions of any function
   *  defined in char_traits, can specialize __gnu_cxx::_Char_types
   *  while leaving __gnu_cxx::char_traits alone. */
  template<typename _CharT>
    struct _Char_types
    {
      typedef unsigned long   int_type;
      typedef std::streampos  pos_type;
      typedef std::streamoff  off_type;
      typedef std::mbstate_t  state_type;
    };


  /**
   *  @brief  Base class used to implement std::char_traits.
   *
   *  @note For any given actual character type, this definition is
   *  probably wrong.  (Most of the member functions are likely to be
   *  right, but the int_type and state_type typedefs, and the eof()
   *  member function, are likely to be wrong.)  The reason this class
   *  exists is so users can specialize it.  Classes in namespace std
   *  may not be specialized for fundamental types, but classes in
   *  namespace __gnu_cxx may be.
   *
   *  See https://gcc.gnu.org/onlinedocs/libstdc++/manual/strings.html#strings.string.character_types
   *  for advice on how to make use of this class for @a unusual character
   *  types. Also, check out include/ext/pod_char_traits.h.  
   */
  template<typename _CharT>
    struct char_traits
    {
      typedef _CharT                                    char_type;
      typedef typename _Char_types<_CharT>::int_type    int_type;
      typedef typename _Char_types<_CharT>::pos_type    pos_type;
      typedef typename _Char_types<_CharT>::off_type    off_type;
      typedef typename _Char_types<_CharT>::state_type  state_type;




      static _GLIBCXX14_CONSTEXPR void
      assign(char_type& __c1, const char_type& __c2)
      { __c1 = __c2; }

      static _GLIBCXX_CONSTEXPR bool
      eq(const char_type& __c1, const char_type& __c2)
      { return __c1 == __c2; }

      static _GLIBCXX_CONSTEXPR bool
      lt(const char_type& __c1, const char_type& __c2)
      { return __c1 < __c2; }

      static _GLIBCXX14_CONSTEXPR int
      compare(const char_type* __s1, const char_type* __s2, std::size_t __n);

      static _GLIBCXX14_CONSTEXPR std::size_t
      length(const char_type* __s);

      static _GLIBCXX14_CONSTEXPR const char_type*
      find(const char_type* __s, std::size_t __n, const char_type& __a);

      static _GLIBCXX20_CONSTEXPR char_type*
      move(char_type* __s1, const char_type* __s2, std::size_t __n);

      static _GLIBCXX20_CONSTEXPR char_type*
      copy(char_type* __s1, const char_type* __s2, std::size_t __n);

      static _GLIBCXX20_CONSTEXPR char_type*
      assign(char_type* __s, std::size_t __n, char_type __a);

      static _GLIBCXX_CONSTEXPR char_type
      to_char_type(const int_type& __c)
      { return static_cast<char_type>(__c); }

      static _GLIBCXX_CONSTEXPR int_type
      to_int_type(const char_type& __c)
      { return static_cast<int_type>(__c); }

      static _GLIBCXX_CONSTEXPR bool
      eq_int_type(const int_type& __c1, const int_type& __c2)
      { return __c1 == __c2; }

      static _GLIBCXX_CONSTEXPR int_type
      eof()
      { return static_cast<int_type>(_GLIBCXX_STDIO_EOF); }

      static _GLIBCXX_CONSTEXPR int_type
      not_eof(const int_type& __c)
      { return !eq_int_type(__c, eof()) ? __c : to_int_type(char_type()); }
    };

  template<typename _CharT>
    _GLIBCXX14_CONSTEXPR int
    char_traits<_CharT>::
    compare(const char_type* __s1, const char_type* __s2, std::size_t __n)
    {
      for (std::size_t __i = 0; __i < __n; ++__i)
	if (lt(__s1[__i], __s2[__i]))
	  return -1;
	else if (lt(__s2[__i], __s1[__i]))
	  return 1;
      return 0;
    }

  template<typename _CharT>
    _GLIBCXX14_CONSTEXPR std::size_t
    char_traits<_CharT>::
    length(const char_type* __p)
    {
      std::size_t __i = 0;
      while (!eq(__p[__i], char_type()))
        ++__i;
      return __i;
    }

  template<typename _CharT>
    _GLIBCXX14_CONSTEXPR const typename char_traits<_CharT>::char_type*
    char_traits<_CharT>::
    find(const char_type* __s, std::size_t __n, const char_type& __a)
    {
      for (std::size_t __i = 0; __i < __n; ++__i)
        if (eq(__s[__i], __a))
          return __s + __i;
      return 0;
    }

  template<typename _CharT>
    _GLIBCXX20_CONSTEXPR
    typename char_traits<_CharT>::char_type*
    char_traits<_CharT>::
    move(char_type* __s1, const char_type* __s2, std::size_t __n)
    {
      if (__n == 0)
	return __s1;
      return static_cast<_CharT*>(__builtin_memmove(__s1, __s2,
						    __n * sizeof(char_type)));
    }

  template<typename _CharT>
    _GLIBCXX20_CONSTEXPR
    typename char_traits<_CharT>::char_type*
    char_traits<_CharT>::
    copy(char_type* __s1, const char_type* __s2, std::size_t __n)
    {
      // NB: Inline std::copy so no recursive dependencies.
      std::copy(__s2, __s2 + __n, __s1);
      return __s1;
    }

  template<typename _CharT>
    _GLIBCXX20_CONSTEXPR
    typename char_traits<_CharT>::char_type*
    char_traits<_CharT>::
    assign(char_type* __s, std::size_t __n, char_type __a)
    {
      // NB: Inline std::fill_n so no recursive dependencies.
      std::fill_n(__s, __n, __a);
      return __s;
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION




// Unofficial macro indicating P0426R1 support
#define __cpp_lib_constexpr_char_traits 201611L





  /**
   *  @brief Determine whether the characters of a NULL-terminated
   *  string are known at compile time.
   *  @param  __s  The string.
   *
   *  Assumes that _CharT is a built-in character type.
   */
  template<typename _CharT>
    static _GLIBCXX_ALWAYS_INLINE constexpr bool
    __constant_string_p(const _CharT* __s)
    {

      (void) __s;
      // In constexpr contexts all strings should be constant.
      return __builtin_is_constant_evaluated();





    }

  /**
   *  @brief Determine whether the characters of a character array are
   *  known at compile time.
   *  @param  __a  The character array.
   *  @param  __n  Number of characters.
   *
   *  Assumes that _CharT is a built-in character type.
   */
  template<typename _CharT>
    static _GLIBCXX_ALWAYS_INLINE constexpr bool
    __constant_char_array_p(const _CharT* __a, size_t __n)
    {

      (void) __a;
      (void) __n;
      // In constexpr contexts all character arrays should be constant.
      return __builtin_is_constant_evaluated();






    }


  // 21.1
  /**
   *  @brief  Basis for explicit traits specializations.
   *
   *  @note  For any given actual character type, this definition is
   *  probably wrong.  Since this is just a thin wrapper around
   *  __gnu_cxx::char_traits, it is possible to achieve a more
   *  appropriate definition by specializing __gnu_cxx::char_traits.
   *
   *  See https://gcc.gnu.org/onlinedocs/libstdc++/manual/strings.html#strings.string.character_types
   *  for advice on how to make use of this class for @a unusual character
   *  types. Also, check out include/ext/pod_char_traits.h.
  */
  template<class _CharT>
    struct char_traits : public __gnu_cxx::char_traits<_CharT>
    { };


  /// 21.1.3.1  char_traits specializations
  template<>
    struct char_traits<char>
    {
      typedef char              char_type;
      typedef int               int_type;
      typedef streampos         pos_type;
      typedef streamoff         off_type;
      typedef mbstate_t         state_type;




      static _GLIBCXX17_CONSTEXPR void
      assign(char_type& __c1, const char_type& __c2) _GLIBCXX_NOEXCEPT
      { __c1 = __c2; }

      static _GLIBCXX_CONSTEXPR bool
      eq(const char_type& __c1, const char_type& __c2) _GLIBCXX_NOEXCEPT
      { return __c1 == __c2; }

      static _GLIBCXX_CONSTEXPR bool
      lt(const char_type& __c1, const char_type& __c2) _GLIBCXX_NOEXCEPT
      {
	// LWG 467.
	return (static_cast<unsigned char>(__c1)
		< static_cast<unsigned char>(__c2));
      }

      static _GLIBCXX17_CONSTEXPR int
      compare(const char_type* __s1, const char_type* __s2, size_t __n)
      {
	if (__n == 0)
	  return 0;

	if (__builtin_constant_p(__n)
	    && __constant_char_array_p(__s1, __n)
	    && __constant_char_array_p(__s2, __n))
	  return __gnu_cxx::char_traits<char_type>::compare(__s1, __s2, __n);

	return __builtin_memcmp(__s1, __s2, __n);
      }

      static _GLIBCXX17_CONSTEXPR size_t
      length(const char_type* __s)
      {

	if (__constant_string_p(__s))
	  return __gnu_cxx::char_traits<char_type>::length(__s);

	return __builtin_strlen(__s);
      }

      static _GLIBCXX17_CONSTEXPR const char_type*
      find(const char_type* __s, size_t __n, const char_type& __a)
      {
	if (__n == 0)
	  return 0;

	if (__builtin_constant_p(__n)
	    && __builtin_constant_p(__a)
	    && __constant_char_array_p(__s, __n))
	  return __gnu_cxx::char_traits<char_type>::find(__s, __n, __a);

	return static_cast<const char_type*>(__builtin_memchr(__s, __a, __n));
      }

      static _GLIBCXX20_CONSTEXPR char_type*
      move(char_type* __s1, const char_type* __s2, size_t __n)
      {
	if (__n == 0)
	  return __s1;




	return static_cast<char_type*>(__builtin_memmove(__s1, __s2, __n));
      }

      static _GLIBCXX20_CONSTEXPR char_type*
      copy(char_type* __s1, const char_type* __s2, size_t __n)
      {
	if (__n == 0)
	  return __s1;




	return static_cast<char_type*>(__builtin_memcpy(__s1, __s2, __n));
      }

      static _GLIBCXX20_CONSTEXPR char_type*
      assign(char_type* __s, size_t __n, char_type __a)
      {
	if (__n == 0)
	  return __s;




	return static_cast<char_type*>(__builtin_memset(__s, __a, __n));
      }

      static _GLIBCXX_CONSTEXPR char_type
      to_char_type(const int_type& __c) _GLIBCXX_NOEXCEPT
      { return static_cast<char_type>(__c); }

      // To keep both the byte 0xff and the eof symbol 0xffffffff
      // from ending up as 0xffffffff.
      static _GLIBCXX_CONSTEXPR int_type
      to_int_type(const char_type& __c) _GLIBCXX_NOEXCEPT
      { return static_cast<int_type>(static_cast<unsigned char>(__c)); }

      static _GLIBCXX_CONSTEXPR bool
      eq_int_type(const int_type& __c1, const int_type& __c2) _GLIBCXX_NOEXCEPT
      { return __c1 == __c2; }

      static _GLIBCXX_CONSTEXPR int_type
      eof() _GLIBCXX_NOEXCEPT
      { return static_cast<int_type>(_GLIBCXX_STDIO_EOF); }

      static _GLIBCXX_CONSTEXPR int_type
      not_eof(const int_type& __c) _GLIBCXX_NOEXCEPT
      { return (__c == eof()) ? 0 : __c; }
  };



  /// 21.1.3.2  char_traits specializations
  template<>
    struct char_traits<wchar_t>
    {
      typedef wchar_t           char_type;
      typedef wint_t            int_type;
      typedef streamoff         off_type;
      typedef wstreampos        pos_type;
      typedef mbstate_t         state_type;




      static _GLIBCXX17_CONSTEXPR void
      assign(char_type& __c1, const char_type& __c2) _GLIBCXX_NOEXCEPT
      { __c1 = __c2; }

      static _GLIBCXX_CONSTEXPR bool
      eq(const char_type& __c1, const char_type& __c2) _GLIBCXX_NOEXCEPT
      { return __c1 == __c2; }

      static _GLIBCXX_CONSTEXPR bool
      lt(const char_type& __c1, const char_type& __c2) _GLIBCXX_NOEXCEPT
      { return __c1 < __c2; }

      static _GLIBCXX17_CONSTEXPR int
      compare(const char_type* __s1, const char_type* __s2, size_t __n)
      {
	if (__n == 0)
	  return 0;

	if (__builtin_constant_p(__n)
	    && __constant_char_array_p(__s1, __n)
	    && __constant_char_array_p(__s2, __n))
	  return __gnu_cxx::char_traits<char_type>::compare(__s1, __s2, __n);

	return wmemcmp(__s1, __s2, __n);
      }

      static _GLIBCXX17_CONSTEXPR size_t
      length(const char_type* __s)
      {

	if (__constant_string_p(__s))
	  return __gnu_cxx::char_traits<char_type>::length(__s);

	return wcslen(__s);
      }

      static _GLIBCXX17_CONSTEXPR const char_type*
      find(const char_type* __s, size_t __n, const char_type& __a)
      {
	if (__n == 0)
	  return 0;

	if (__builtin_constant_p(__n)
	    && __builtin_constant_p(__a)
	    && __constant_char_array_p(__s, __n))
	  return __gnu_cxx::char_traits<char_type>::find(__s, __n, __a);

	return wmemchr(__s, __a, __n);
      }

      static _GLIBCXX20_CONSTEXPR char_type*
      move(char_type* __s1, const char_type* __s2, size_t __n)
      {
	if (__n == 0)
	  return __s1;




	return wmemmove(__s1, __s2, __n);
      }

      static _GLIBCXX20_CONSTEXPR char_type*
      copy(char_type* __s1, const char_type* __s2, size_t __n)
      {
	if (__n == 0)
	  return __s1;




	return wmemcpy(__s1, __s2, __n);
      }

      static _GLIBCXX20_CONSTEXPR char_type*
      assign(char_type* __s, size_t __n, char_type __a)
      {
	if (__n == 0)
	  return __s;




	return wmemset(__s, __a, __n);
      }

      static _GLIBCXX_CONSTEXPR char_type
      to_char_type(const int_type& __c) _GLIBCXX_NOEXCEPT
      { return char_type(__c); }

      static _GLIBCXX_CONSTEXPR int_type
      to_int_type(const char_type& __c) _GLIBCXX_NOEXCEPT
      { return int_type(__c); }

      static _GLIBCXX_CONSTEXPR bool
      eq_int_type(const int_type& __c1, const int_type& __c2) _GLIBCXX_NOEXCEPT
      { return __c1 == __c2; }

      static _GLIBCXX_CONSTEXPR int_type
      eof() _GLIBCXX_NOEXCEPT
      { return static_cast<int_type>(WEOF); }

      static _GLIBCXX_CONSTEXPR int_type
      not_eof(const int_type& __c) _GLIBCXX_NOEXCEPT
      { return eq_int_type(__c, eof()) ? 0 : __c; }
  };



_GLIBCXX_END_NAMESPACE_VERSION
} // namespace



// <cstdint> -*- C++ -*-

// Copyright (C) 2007-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/cstdint
 *  This is a Standard C++ Library header.
 */


#define _GLIBCXX_CSTDINT 1

       











#undef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS 
#undef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS 

/* Copyright (C) 1997-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99: 7.18 Integer types <stdint.h>
 */


#define _STDINT_H 1

#define __GLIBC_INTERNAL_STARTING_HEADER_IMPLEMENTATION 
/* Handle feature test macros at the start of a header.
   Copyright (C) 2016-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* This header is internal to glibc and should not be included outside
   of glibc headers.  Headers including it must define
   __GLIBC_INTERNAL_STARTING_HEADER_IMPLEMENTATION first.  This header
   cannot have multiple include guards because ISO C feature test
   macros depend on the definition of the macro when an affected
   header is included, not when the first system header is
   included.  */





#undef __GLIBC_INTERNAL_STARTING_HEADER_IMPLEMENTATION



/* ISO/IEC TR 24731-2:2010 defines the __STDC_WANT_LIB_EXT2__
   macro.  */
#undef __GLIBC_USE_LIB_EXT2


#define __GLIBC_USE_LIB_EXT2 1




/* ISO/IEC TS 18661-1:2014 defines the __STDC_WANT_IEC_60559_BFP_EXT__
   macro.  */
#undef __GLIBC_USE_IEC_60559_BFP_EXT

#define __GLIBC_USE_IEC_60559_BFP_EXT 1




/* ISO/IEC TS 18661-4:2015 defines the
   __STDC_WANT_IEC_60559_FUNCS_EXT__ macro.  */
#undef __GLIBC_USE_IEC_60559_FUNCS_EXT

#define __GLIBC_USE_IEC_60559_FUNCS_EXT 1




/* ISO/IEC TS 18661-3:2015 defines the
   __STDC_WANT_IEC_60559_TYPES_EXT__ macro.  */
#undef __GLIBC_USE_IEC_60559_TYPES_EXT

#define __GLIBC_USE_IEC_60559_TYPES_EXT 1
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */


#define _BITS_TYPES_H 1


/* Determine the wordsize from the preprocessor defines.  */


#define __WORDSIZE 64







#define __WORDSIZE_TIME64_COMPAT32 1
/* Both x86-64 and x32 use the 64-bit system call interface.  */
#define __SYSCALL_WORDSIZE 64

/* Convenience types.  */
typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;

/* Fixed-size types, underlying types depend on word size and compiler.  */
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;





/* Smallest types with at least a given width.  */
typedef __int8_t __int_least8_t;
typedef __uint8_t __uint_least8_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef __int64_t __int_least64_t;
typedef __uint64_t __uint_least64_t;

/* quad_t is also 64 bits.  */

typedef long int __quad_t;
typedef unsigned long int __u_quad_t;





/* Largest integral types.  */

typedef long int __intmax_t;
typedef unsigned long int __uintmax_t;






/* The machine-dependent file <bits/typesizes.h> defines __*_T_TYPE
   macros for each of the OS types we define below.  The definitions
   of those macros must use the following macros for underlying types.
   We define __S<SIZE>_TYPE and __U<SIZE>_TYPE for the signed and unsigned
   variants of each of the following integer types on this machine.

	16		-- "natural" 16-bit type (always short)
	32		-- "natural" 32-bit type (always int)
	64		-- "natural" 64-bit type (long or long long)
	LONG32		-- 32-bit type, traditionally long
	QUAD		-- 64-bit type, traditionally long long
	WORD		-- natural type of __WORDSIZE bits (int or long)
	LONGWORD	-- type of __WORDSIZE bits, traditionally long

   We distinguish WORD/LONGWORD, 32/LONG32, and 64/QUAD so that the
   conventional uses of `long' or `long long' type modifiers match the
   types we define, even when a less-adorned type would be the same size.
   This matters for (somewhat) portably writing printf/scanf formats for
   these types, where using the appropriate l or ll format modifiers can
   make the typedefs and the formats match up across all GNU platforms.  If
   we used `long' when it's 64 bits where `long long' is expected, then the
   compiler would warn about the formats not matching the argument types,
   and the programmer changing them to shut up the compiler would break the
   program's portability.

   Here we assume what is presently the case in all the GCC configurations
   we support: long long is always 64 bits, long is always word/address size,
   and int is always 32 bits.  */

#define __S16_TYPE short int
#define __U16_TYPE unsigned short int
#define __S32_TYPE int
#define __U32_TYPE unsigned int
#define __SLONGWORD_TYPE long int
#define __ULONGWORD_TYPE unsigned long int
#define __SQUAD_TYPE long int
#define __UQUAD_TYPE unsigned long int
#define __SWORD_TYPE long int
#define __UWORD_TYPE unsigned long int
#define __SLONG32_TYPE int
#define __ULONG32_TYPE unsigned int
#define __S64_TYPE long int
#define __U64_TYPE unsigned long int
/* No need to mark the typedef with __extension__.   */
#define __STD_TYPE typedef



/* bits/typesizes.h -- underlying types for *_t.  Linux/x86-64 version.
   Copyright (C) 2012-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */






#define _BITS_TYPESIZES_H 1

/* See <bits/types.h> for the meaning of these macros.  This file exists so
   that <bits/types.h> need not vary across different GNU platforms.  */

/* X32 kernel interface is 64-bit.  */




#define __SYSCALL_SLONG_TYPE __SLONGWORD_TYPE
#define __SYSCALL_ULONG_TYPE __ULONGWORD_TYPE


#define __DEV_T_TYPE __UQUAD_TYPE
#define __UID_T_TYPE __U32_TYPE
#define __GID_T_TYPE __U32_TYPE
#define __INO_T_TYPE __SYSCALL_ULONG_TYPE
#define __INO64_T_TYPE __UQUAD_TYPE
#define __MODE_T_TYPE __U32_TYPE

#define __NLINK_T_TYPE __SYSCALL_ULONG_TYPE
#define __FSWORD_T_TYPE __SYSCALL_SLONG_TYPE




#define __OFF_T_TYPE __SYSCALL_SLONG_TYPE
#define __OFF64_T_TYPE __SQUAD_TYPE
#define __PID_T_TYPE __S32_TYPE
#define __RLIM_T_TYPE __SYSCALL_ULONG_TYPE
#define __RLIM64_T_TYPE __UQUAD_TYPE
#define __BLKCNT_T_TYPE __SYSCALL_SLONG_TYPE
#define __BLKCNT64_T_TYPE __SQUAD_TYPE
#define __FSBLKCNT_T_TYPE __SYSCALL_ULONG_TYPE
#define __FSBLKCNT64_T_TYPE __UQUAD_TYPE
#define __FSFILCNT_T_TYPE __SYSCALL_ULONG_TYPE
#define __FSFILCNT64_T_TYPE __UQUAD_TYPE
#define __ID_T_TYPE __U32_TYPE
#define __CLOCK_T_TYPE __SYSCALL_SLONG_TYPE
#define __TIME_T_TYPE __SYSCALL_SLONG_TYPE
#define __USECONDS_T_TYPE __U32_TYPE
#define __SUSECONDS_T_TYPE __SYSCALL_SLONG_TYPE
#define __DADDR_T_TYPE __S32_TYPE
#define __KEY_T_TYPE __S32_TYPE
#define __CLOCKID_T_TYPE __S32_TYPE
#define __TIMER_T_TYPE void *
#define __BLKSIZE_T_TYPE __SYSCALL_SLONG_TYPE
#define __FSID_T_TYPE struct { int __val[2]; }
#define __SSIZE_T_TYPE __SWORD_TYPE
#define __CPU_MASK_TYPE __SYSCALL_ULONG_TYPE


/* Tell the libc code that off_t and off64_t are actually the same type
   for all ABI purposes, even if possibly expressed as different base types
   for C type-checking purposes.  */
#define __OFF_T_MATCHES_OFF64_T 1

/* Same for ino_t and ino64_t.  */
#define __INO_T_MATCHES_INO64_T 1

/* And for __rlim_t and __rlim64_t.  */
#define __RLIM_T_MATCHES_RLIM64_T 1




/* Number of descriptors that can fit in an `fd_set'.  */
#define __FD_SETSIZE 1024




__STD_TYPE __DEV_T_TYPE __dev_t;	/* Type of device numbers.  */
__STD_TYPE __UID_T_TYPE __uid_t;	/* Type of user identifications.  */
__STD_TYPE __GID_T_TYPE __gid_t;	/* Type of group identifications.  */
__STD_TYPE __INO_T_TYPE __ino_t;	/* Type of file serial numbers.  */
__STD_TYPE __INO64_T_TYPE __ino64_t;	/* Type of file serial numbers (LFS).*/
__STD_TYPE __MODE_T_TYPE __mode_t;	/* Type of file attribute bitmasks.  */
__STD_TYPE __NLINK_T_TYPE __nlink_t;	/* Type of file link counts.  */
__STD_TYPE __OFF_T_TYPE __off_t;	/* Type of file sizes and offsets.  */
__STD_TYPE __OFF64_T_TYPE __off64_t;	/* Type of file sizes and offsets (LFS).  */
__STD_TYPE __PID_T_TYPE __pid_t;	/* Type of process identifications.  */
__STD_TYPE __FSID_T_TYPE __fsid_t;	/* Type of file system IDs.  */
__STD_TYPE __CLOCK_T_TYPE __clock_t;	/* Type of CPU usage counts.  */
__STD_TYPE __RLIM_T_TYPE __rlim_t;	/* Type for resource measurement.  */
__STD_TYPE __RLIM64_T_TYPE __rlim64_t;	/* Type for resource measurement (LFS).  */
__STD_TYPE __ID_T_TYPE __id_t;		/* General type for IDs.  */
__STD_TYPE __TIME_T_TYPE __time_t;	/* Seconds since the Epoch.  */
__STD_TYPE __USECONDS_T_TYPE __useconds_t; /* Count of microseconds.  */
__STD_TYPE __SUSECONDS_T_TYPE __suseconds_t; /* Signed count of microseconds.  */

__STD_TYPE __DADDR_T_TYPE __daddr_t;	/* The type of a disk address.  */
__STD_TYPE __KEY_T_TYPE __key_t;	/* Type of an IPC key.  */

/* Clock ID used in clock and timer functions.  */
__STD_TYPE __CLOCKID_T_TYPE __clockid_t;

/* Timer ID returned by `timer_create'.  */
__STD_TYPE __TIMER_T_TYPE __timer_t;

/* Type to represent block size.  */
__STD_TYPE __BLKSIZE_T_TYPE __blksize_t;

/* Types from the Large File Support interface.  */

/* Type to count number of disk blocks.  */
__STD_TYPE __BLKCNT_T_TYPE __blkcnt_t;
__STD_TYPE __BLKCNT64_T_TYPE __blkcnt64_t;

/* Type to count file system blocks.  */
__STD_TYPE __FSBLKCNT_T_TYPE __fsblkcnt_t;
__STD_TYPE __FSBLKCNT64_T_TYPE __fsblkcnt64_t;

/* Type to count file system nodes.  */
__STD_TYPE __FSFILCNT_T_TYPE __fsfilcnt_t;
__STD_TYPE __FSFILCNT64_T_TYPE __fsfilcnt64_t;

/* Type of miscellaneous file system fields.  */
__STD_TYPE __FSWORD_T_TYPE __fsword_t;

__STD_TYPE __SSIZE_T_TYPE __ssize_t; /* Type of a byte count, or error.  */

/* Signed long type used in system calls.  */
__STD_TYPE __SYSCALL_SLONG_TYPE __syscall_slong_t;
/* Unsigned long type used in system calls.  */
__STD_TYPE __SYSCALL_ULONG_TYPE __syscall_ulong_t;

/* These few don't really vary by system, they always correspond
   to one of the other defined types.  */
typedef __off64_t __loff_t;	/* Type of file sizes and offsets (LFS).  */
typedef char *__caddr_t;

/* Duplicates info from stdint.h but this is used in unistd.h.  */
__STD_TYPE __SWORD_TYPE __intptr_t;

/* Duplicate info from sys/socket.h.  */
__STD_TYPE __U32_TYPE __socklen_t;

/* C99: An integer type that can be accessed as an atomic entity,
   even in the presence of asynchronous interrupts.
   It is not currently necessary for this to be machine-specific.  */
typedef int __sig_atomic_t;

#undef __STD_TYPE


/* Determine the wordsize from the preprocessor defines.  */


#define __WORDSIZE 64







#define __WORDSIZE_TIME64_COMPAT32 1
/* Both x86-64 and x32 use the 64-bit system call interface.  */
#define __SYSCALL_WORDSIZE 64

/* Exact integral types.  */

/* Signed.  */
/* Define intN_t types.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_STDINT_INTN_H 1



typedef __int8_t int8_t;
typedef __int16_t int16_t;
typedef __int32_t int32_t;
typedef __int64_t int64_t;


/* Unsigned.  */
/* Define uintN_t types.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_STDINT_UINTN_H 1



typedef __uint8_t uint8_t;
typedef __uint16_t uint16_t;
typedef __uint32_t uint32_t;
typedef __uint64_t uint64_t;



/* Small types.  */

/* Signed.  */
typedef __int_least8_t int_least8_t;
typedef __int_least16_t int_least16_t;
typedef __int_least32_t int_least32_t;
typedef __int_least64_t int_least64_t;

/* Unsigned.  */
typedef __uint_least8_t uint_least8_t;
typedef __uint_least16_t uint_least16_t;
typedef __uint_least32_t uint_least32_t;
typedef __uint_least64_t uint_least64_t;


/* Fast types.  */

/* Signed.  */
typedef signed char		int_fast8_t;

typedef long int		int_fast16_t;
typedef long int		int_fast32_t;
typedef long int		int_fast64_t;







/* Unsigned.  */
typedef unsigned char		uint_fast8_t;

typedef unsigned long int	uint_fast16_t;
typedef unsigned long int	uint_fast32_t;
typedef unsigned long int	uint_fast64_t;








/* Types for `void *' pointers.  */


typedef long int		intptr_t;
#define __intptr_t_defined 

typedef unsigned long int	uintptr_t;









/* Largest integral types.  */
typedef __intmax_t		intmax_t;
typedef __uintmax_t		uintmax_t;



#define __INT64_C(c) c ## L
#define __UINT64_C(c) c ## UL





/* Limits of integral types.  */

/* Minimum of signed integral types.  */
#define INT8_MIN (-128)
#define INT16_MIN (-32767-1)
#define INT32_MIN (-2147483647-1)
#define INT64_MIN (-__INT64_C(9223372036854775807)-1)
/* Maximum of signed integral types.  */
#define INT8_MAX (127)
#define INT16_MAX (32767)
#define INT32_MAX (2147483647)
#define INT64_MAX (__INT64_C(9223372036854775807))

/* Maximum of unsigned integral types.  */
#define UINT8_MAX (255)
#define UINT16_MAX (65535)
#define UINT32_MAX (4294967295U)
#define UINT64_MAX (__UINT64_C(18446744073709551615))


/* Minimum of signed integral types having a minimum size.  */
#define INT_LEAST8_MIN (-128)
#define INT_LEAST16_MIN (-32767-1)
#define INT_LEAST32_MIN (-2147483647-1)
#define INT_LEAST64_MIN (-__INT64_C(9223372036854775807)-1)
/* Maximum of signed integral types having a minimum size.  */
#define INT_LEAST8_MAX (127)
#define INT_LEAST16_MAX (32767)
#define INT_LEAST32_MAX (2147483647)
#define INT_LEAST64_MAX (__INT64_C(9223372036854775807))

/* Maximum of unsigned integral types having a minimum size.  */
#define UINT_LEAST8_MAX (255)
#define UINT_LEAST16_MAX (65535)
#define UINT_LEAST32_MAX (4294967295U)
#define UINT_LEAST64_MAX (__UINT64_C(18446744073709551615))


/* Minimum of fast signed integral types having a minimum size.  */
#define INT_FAST8_MIN (-128)

#define INT_FAST16_MIN (-9223372036854775807L-1)
#define INT_FAST32_MIN (-9223372036854775807L-1)




#define INT_FAST64_MIN (-__INT64_C(9223372036854775807)-1)
/* Maximum of fast signed integral types having a minimum size.  */
#define INT_FAST8_MAX (127)

#define INT_FAST16_MAX (9223372036854775807L)
#define INT_FAST32_MAX (9223372036854775807L)




#define INT_FAST64_MAX (__INT64_C(9223372036854775807))

/* Maximum of fast unsigned integral types having a minimum size.  */
#define UINT_FAST8_MAX (255)

#define UINT_FAST16_MAX (18446744073709551615UL)
#define UINT_FAST32_MAX (18446744073709551615UL)




#define UINT_FAST64_MAX (__UINT64_C(18446744073709551615))


/* Values to test for integral types holding `void *' pointer.  */

#define INTPTR_MIN (-9223372036854775807L-1)
#define INTPTR_MAX (9223372036854775807L)
#define UINTPTR_MAX (18446744073709551615UL)







/* Minimum for largest signed integral type.  */
#define INTMAX_MIN (-__INT64_C(9223372036854775807)-1)
/* Maximum for largest signed integral type.  */
#define INTMAX_MAX (__INT64_C(9223372036854775807))

/* Maximum for largest unsigned integral type.  */
#define UINTMAX_MAX (__UINT64_C(18446744073709551615))


/* Limits of other integer types.  */

/* Limits of `ptrdiff_t' type.  */

#define PTRDIFF_MIN (-9223372036854775807L-1)
#define PTRDIFF_MAX (9223372036854775807L)

/* Limits of `sig_atomic_t'.  */
#define SIG_ATOMIC_MIN (-2147483647-1)
#define SIG_ATOMIC_MAX (2147483647)

/* Limit of `size_t' type.  */

#define SIZE_MAX (18446744073709551615UL)








/* Limits of `wchar_t'.  */






/* Limits of `wint_t'.  */
#define WINT_MIN (0u)
#define WINT_MAX (4294967295u)

/* Signed.  */
#define INT8_C(c) c
#define INT16_C(c) c
#define INT32_C(c) c

#define INT64_C(c) c ## L




/* Unsigned.  */
#define UINT8_C(c) c
#define UINT16_C(c) c
#define UINT32_C(c) c ## U

#define UINT64_C(c) c ## UL




/* Maximal type.  */

#define INTMAX_C(c) c ## L
#define UINTMAX_C(c) c ## UL







#define INT8_WIDTH 8
#define UINT8_WIDTH 8
#define INT16_WIDTH 16
#define UINT16_WIDTH 16
#define INT32_WIDTH 32
#define UINT32_WIDTH 32
#define INT64_WIDTH 64
#define UINT64_WIDTH 64

#define INT_LEAST8_WIDTH 8
#define UINT_LEAST8_WIDTH 8
#define INT_LEAST16_WIDTH 16
#define UINT_LEAST16_WIDTH 16
#define INT_LEAST32_WIDTH 32
#define UINT_LEAST32_WIDTH 32
#define INT_LEAST64_WIDTH 64
#define UINT_LEAST64_WIDTH 64

#define INT_FAST8_WIDTH 8
#define UINT_FAST8_WIDTH 8
#define INT_FAST16_WIDTH __WORDSIZE
#define UINT_FAST16_WIDTH __WORDSIZE
#define INT_FAST32_WIDTH __WORDSIZE
#define UINT_FAST32_WIDTH __WORDSIZE
#define INT_FAST64_WIDTH 64
#define UINT_FAST64_WIDTH 64

#define INTPTR_WIDTH __WORDSIZE
#define UINTPTR_WIDTH __WORDSIZE

#define INTMAX_WIDTH 64
#define UINTMAX_WIDTH 64

#define PTRDIFF_WIDTH __WORDSIZE
#define SIG_ATOMIC_WIDTH 32
#define SIZE_WIDTH __WORDSIZE
#define WCHAR_WIDTH 32
#define WINT_WIDTH 32






#define _GCC_WRAP_STDINT_H 


namespace std
{

  using ::int8_t;
  using ::int16_t;
  using ::int32_t;
  using ::int64_t;

  using ::int_fast8_t;
  using ::int_fast16_t;
  using ::int_fast32_t;
  using ::int_fast64_t;

  using ::int_least8_t;
  using ::int_least16_t;
  using ::int_least32_t;
  using ::int_least64_t;

  using ::intmax_t;
  using ::intptr_t;
  
  using ::uint8_t;
  using ::uint16_t;
  using ::uint32_t;
  using ::uint64_t;

  using ::uint_fast8_t;
  using ::uint_fast16_t;
  using ::uint_fast32_t;
  using ::uint_fast64_t;

  using ::uint_least8_t;
  using ::uint_least16_t;
  using ::uint_least32_t;
  using ::uint_least64_t;

  using ::uintmax_t;
  using ::uintptr_t;





} // namespace std




namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<>
    struct char_traits<char16_t>
    {
      typedef char16_t          char_type;

      typedef uint_least16_t    int_type;





      typedef streamoff         off_type;
      typedef u16streampos      pos_type;
      typedef mbstate_t         state_type;




      static _GLIBCXX17_CONSTEXPR void
      assign(char_type& __c1, const char_type& __c2) noexcept
      { __c1 = __c2; }

      static constexpr bool
      eq(const char_type& __c1, const char_type& __c2) noexcept
      { return __c1 == __c2; }

      static constexpr bool
      lt(const char_type& __c1, const char_type& __c2) noexcept
      { return __c1 < __c2; }

      static _GLIBCXX17_CONSTEXPR int
      compare(const char_type* __s1, const char_type* __s2, size_t __n)
      {
	for (size_t __i = 0; __i < __n; ++__i)
	  if (lt(__s1[__i], __s2[__i]))
	    return -1;
	  else if (lt(__s2[__i], __s1[__i]))
	    return 1;
	return 0;
      }

      static _GLIBCXX17_CONSTEXPR size_t
      length(const char_type* __s)
      {
	size_t __i = 0;
	while (!eq(__s[__i], char_type()))
	  ++__i;
	return __i;
      }

      static _GLIBCXX17_CONSTEXPR const char_type*
      find(const char_type* __s, size_t __n, const char_type& __a)
      {
	for (size_t __i = 0; __i < __n; ++__i)
	  if (eq(__s[__i], __a))
	    return __s + __i;
	return 0;
      }

      static _GLIBCXX20_CONSTEXPR char_type*
      move(char_type* __s1, const char_type* __s2, size_t __n)
      {
	if (__n == 0)
	  return __s1;




	return (static_cast<char_type*>
		(__builtin_memmove(__s1, __s2, __n * sizeof(char_type))));
      }

      static _GLIBCXX20_CONSTEXPR char_type*
      copy(char_type* __s1, const char_type* __s2, size_t __n)
      {
	if (__n == 0)
	  return __s1;




	return (static_cast<char_type*>
		(__builtin_memcpy(__s1, __s2, __n * sizeof(char_type))));
      }

      static _GLIBCXX20_CONSTEXPR char_type*
      assign(char_type* __s, size_t __n, char_type __a)
      {
	for (size_t __i = 0; __i < __n; ++__i)
	  assign(__s[__i], __a);
	return __s;
      }

      static constexpr char_type
      to_char_type(const int_type& __c) noexcept
      { return char_type(__c); }

      static constexpr int_type
      to_int_type(const char_type& __c) noexcept
      { return __c == eof() ? int_type(0xfffd) : int_type(__c); }

      static constexpr bool
      eq_int_type(const int_type& __c1, const int_type& __c2) noexcept
      { return __c1 == __c2; }

      static constexpr int_type
      eof() noexcept
      { return static_cast<int_type>(-1); }

      static constexpr int_type
      not_eof(const int_type& __c) noexcept
      { return eq_int_type(__c, eof()) ? 0 : __c; }
    };

  template<>
    struct char_traits<char32_t>
    {
      typedef char32_t          char_type;

      typedef uint_least32_t    int_type;





      typedef streamoff         off_type;
      typedef u32streampos      pos_type;
      typedef mbstate_t         state_type;




      static _GLIBCXX17_CONSTEXPR void
      assign(char_type& __c1, const char_type& __c2) noexcept
      { __c1 = __c2; }

      static constexpr bool
      eq(const char_type& __c1, const char_type& __c2) noexcept
      { return __c1 == __c2; }

      static constexpr bool
      lt(const char_type& __c1, const char_type& __c2) noexcept
      { return __c1 < __c2; }

      static _GLIBCXX17_CONSTEXPR int
      compare(const char_type* __s1, const char_type* __s2, size_t __n)
      {
	for (size_t __i = 0; __i < __n; ++__i)
	  if (lt(__s1[__i], __s2[__i]))
	    return -1;
	  else if (lt(__s2[__i], __s1[__i]))
	    return 1;
	return 0;
      }

      static _GLIBCXX17_CONSTEXPR size_t
      length(const char_type* __s)
      {
	size_t __i = 0;
	while (!eq(__s[__i], char_type()))
	  ++__i;
	return __i;
      }

      static _GLIBCXX17_CONSTEXPR const char_type*
      find(const char_type* __s, size_t __n, const char_type& __a)
      {
	for (size_t __i = 0; __i < __n; ++__i)
	  if (eq(__s[__i], __a))
	    return __s + __i;
	return 0;
      }

      static _GLIBCXX20_CONSTEXPR char_type*
      move(char_type* __s1, const char_type* __s2, size_t __n)
      {
	if (__n == 0)
	  return __s1;




	return (static_cast<char_type*>
		(__builtin_memmove(__s1, __s2, __n * sizeof(char_type))));
      }

      static _GLIBCXX20_CONSTEXPR char_type*
      copy(char_type* __s1, const char_type* __s2, size_t __n)
      { 
	if (__n == 0)
	  return __s1;




	return (static_cast<char_type*>
		(__builtin_memcpy(__s1, __s2, __n * sizeof(char_type))));
      }

      static _GLIBCXX20_CONSTEXPR char_type*
      assign(char_type* __s, size_t __n, char_type __a)
      {
	for (size_t __i = 0; __i < __n; ++__i)
	  assign(__s[__i], __a);
	return __s;
      }

      static constexpr char_type
      to_char_type(const int_type& __c) noexcept
      { return char_type(__c); }

      static constexpr int_type
      to_int_type(const char_type& __c) noexcept
      { return int_type(__c); }

      static constexpr bool
      eq_int_type(const int_type& __c1, const int_type& __c2) noexcept
      { return __c1 == __c2; }

      static constexpr int_type
      eof() noexcept
      { return static_cast<int_type>(-1); }

      static constexpr int_type
      not_eof(const int_type& __c) noexcept
      { return eq_int_type(__c, eof()) ? 0 : __c; }
    };


_GLIBCXX_END_NAMESPACE_VERSION
} // namespace



// <locale> Forward declarations -*- C++ -*-

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/localefwd.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{locale}
 */

//
// ISO C++ 14882: 22.1  Locales
//


#define _LOCALE_FWD_H 1

       


// Wrapper for underlying C-language localization -*- C++ -*-

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/c++locale.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{locale}
 */

//
// ISO C++ 14882: 22.8  Standard locale categories.
//

// Written by Benjamin Kosnik <bkoz@redhat.com>


#define _GLIBCXX_CXX_LOCALE_H 1

       

// -*- C++ -*- forwarding header.

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file clocale
 *  This is a Standard C++ Library file.  You should @c \#include this file
 *  in your programs, rather than any of the @a *.h implementation files.
 *
 *  This is the C++ version of the Standard C Library header @c locale.h,
 *  and its contents are (mostly) the same as that header, but are all
 *  contained in the namespace @c std (except for names which are defined
 *  as macros in C).
 */

//
// ISO C++ 14882: 18.2.2  Implementation properties: C library
//

       


/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99 Standard: 7.11 Localization	<locale.h>
 */


#define _LOCALE_H 1



#define __need_NULL 
/* Copyright (C) 1989-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */






/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */


/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */










/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */




/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */













/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */

/* Signed type of difference of two pointers.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


/* If this symbol has done its job, get rid of it.  */
#undef __need_ptrdiff_t



/* Unsigned type of `sizeof' something.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

#undef __need_size_t



/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

#undef __need_wchar_t






/* A null pointer constant.  */


#undef NULL

#define NULL __null








#undef __need_NULL



/* Offset of member MEMBER in a struct of type TYPE. */
#define offsetof(TYPE,MEMBER) __builtin_offsetof (TYPE, MEMBER)














/* Definition of locale category symbol values.
   Copyright (C) 2001-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */






#define _BITS_LOCALE_H 1

#define __LC_CTYPE 0
#define __LC_NUMERIC 1
#define __LC_TIME 2
#define __LC_COLLATE 3
#define __LC_MONETARY 4
#define __LC_MESSAGES 5
#define __LC_ALL 6
#define __LC_PAPER 7
#define __LC_NAME 8
#define __LC_ADDRESS 9
#define __LC_TELEPHONE 10
#define __LC_MEASUREMENT 11
#define __LC_IDENTIFICATION 12


__BEGIN_DECLS

/* These are the possibilities for the first argument to setlocale.
   The code assumes that the lowest LC_* symbol has the value zero.  */
#define LC_CTYPE __LC_CTYPE
#define LC_NUMERIC __LC_NUMERIC
#define LC_TIME __LC_TIME
#define LC_COLLATE __LC_COLLATE
#define LC_MONETARY __LC_MONETARY
#define LC_MESSAGES __LC_MESSAGES
#define LC_ALL __LC_ALL
#define LC_PAPER __LC_PAPER
#define LC_NAME __LC_NAME
#define LC_ADDRESS __LC_ADDRESS
#define LC_TELEPHONE __LC_TELEPHONE
#define LC_MEASUREMENT __LC_MEASUREMENT
#define LC_IDENTIFICATION __LC_IDENTIFICATION


/* Structure giving information about numeric and monetary notation.  */
struct lconv
{
  /* Numeric (non-monetary) information.  */

  char *decimal_point;		/* Decimal point character.  */
  char *thousands_sep;		/* Thousands separator.  */
  /* Each element is the number of digits in each group;
     elements with higher indices are farther left.
     An element with value CHAR_MAX means that no further grouping is done.
     An element with value 0 means that the previous element is used
     for all groups farther left.  */
  char *grouping;

  /* Monetary information.  */

  /* First three chars are a currency symbol from ISO 4217.
     Fourth char is the separator.  Fifth char is '\0'.  */
  char *int_curr_symbol;
  char *currency_symbol;	/* Local currency symbol.  */
  char *mon_decimal_point;	/* Decimal point character.  */
  char *mon_thousands_sep;	/* Thousands separator.  */
  char *mon_grouping;		/* Like `grouping' element (above).  */
  char *positive_sign;		/* Sign for positive values.  */
  char *negative_sign;		/* Sign for negative values.  */
  char int_frac_digits;		/* Int'l fractional digits.  */
  char frac_digits;		/* Local fractional digits.  */
  /* 1 if currency_symbol precedes a positive value, 0 if succeeds.  */
  char p_cs_precedes;
  /* 1 iff a space separates currency_symbol from a positive value.  */
  char p_sep_by_space;
  /* 1 if currency_symbol precedes a negative value, 0 if succeeds.  */
  char n_cs_precedes;
  /* 1 iff a space separates currency_symbol from a negative value.  */
  char n_sep_by_space;
  /* Positive and negative sign positions:
     0 Parentheses surround the quantity and currency_symbol.
     1 The sign string precedes the quantity and currency_symbol.
     2 The sign string follows the quantity and currency_symbol.
     3 The sign string immediately precedes the currency_symbol.
     4 The sign string immediately follows the currency_symbol.  */
  char p_sign_posn;
  char n_sign_posn;

  /* 1 if int_curr_symbol precedes a positive value, 0 if succeeds.  */
  char int_p_cs_precedes;
  /* 1 iff a space separates int_curr_symbol from a positive value.  */
  char int_p_sep_by_space;
  /* 1 if int_curr_symbol precedes a negative value, 0 if succeeds.  */
  char int_n_cs_precedes;
  /* 1 iff a space separates int_curr_symbol from a negative value.  */
  char int_n_sep_by_space;
  /* Positive and negative sign positions:
     0 Parentheses surround the quantity and int_curr_symbol.
     1 The sign string precedes the quantity and int_curr_symbol.
     2 The sign string follows the quantity and int_curr_symbol.
     3 The sign string immediately precedes the int_curr_symbol.
     4 The sign string immediately follows the int_curr_symbol.  */
  char int_p_sign_posn;
  char int_n_sign_posn;
};


/* Set and/or return the current locale.  */
extern char *setlocale (int __category, const char *__locale) __THROW;

/* Return the numeric/monetary information for the current locale.  */
extern struct lconv *localeconv (void) __THROW;



/* POSIX.1-2008 extends the locale interface with functions for
   explicit creation and manipulation of 'locale_t' objects
   representing locale contexts, and a set of parallel
   locale-sensitive text processing functions that take a locale_t
   argument.  This enables applications to work with data from
   multiple locales simultaneously and thread-safely.  */


/* Return a reference to a data structure representing a set of locale
   datasets.  Unlike for the CATEGORY parameter for `setlocale' the
   CATEGORY_MASK parameter here uses a single bit for each category,
   made by OR'ing together LC_*_MASK bits above.  */
extern locale_t newlocale (int __category_mask, const char *__locale,
			   locale_t __base) __THROW;

/* These are the bits that can be set in the CATEGORY_MASK argument to
   `newlocale'.  In the GNU implementation, LC_FOO_MASK has the value
   of (1 << LC_FOO), but this is not a part of the interface that
   callers can assume will be true.  */
#define LC_CTYPE_MASK (1 << __LC_CTYPE)
#define LC_NUMERIC_MASK (1 << __LC_NUMERIC)
#define LC_TIME_MASK (1 << __LC_TIME)
#define LC_COLLATE_MASK (1 << __LC_COLLATE)
#define LC_MONETARY_MASK (1 << __LC_MONETARY)
#define LC_MESSAGES_MASK (1 << __LC_MESSAGES)
#define LC_PAPER_MASK (1 << __LC_PAPER)
#define LC_NAME_MASK (1 << __LC_NAME)
#define LC_ADDRESS_MASK (1 << __LC_ADDRESS)
#define LC_TELEPHONE_MASK (1 << __LC_TELEPHONE)
#define LC_MEASUREMENT_MASK (1 << __LC_MEASUREMENT)
#define LC_IDENTIFICATION_MASK (1 << __LC_IDENTIFICATION)
#define LC_ALL_MASK (LC_CTYPE_MASK | LC_NUMERIC_MASK | LC_TIME_MASK | LC_COLLATE_MASK | LC_MONETARY_MASK | LC_MESSAGES_MASK | LC_PAPER_MASK | LC_NAME_MASK | LC_ADDRESS_MASK | LC_TELEPHONE_MASK | LC_MEASUREMENT_MASK | LC_IDENTIFICATION_MASK )

/* Return a duplicate of the set of locale in DATASET.  All usage
   counters are increased if necessary.  */
extern locale_t duplocale (locale_t __dataset) __THROW;

/* Free the data associated with a locale dataset previously returned
   by a call to `setlocale_r'.  */
extern void freelocale (locale_t __dataset) __THROW;

/* Switch the current thread's locale to DATASET.
   If DATASET is null, instead just return the current setting.
   The special value LC_GLOBAL_LOCALE is the initial setting
   for all threads and can also be installed any time, meaning
   the thread uses the global settings controlled by `setlocale'.  */
extern locale_t uselocale (locale_t __dataset) __THROW;

/* This value can be passed to `uselocale' and may be returned by it.
   Passing this value to any other function has undefined behavior.  */
#define LC_GLOBAL_LOCALE ((locale_t) -1L)



__END_DECLS



#define _GLIBCXX_CLOCALE 1

// Get rid of those macros defined in <locale.h> in lieu of real functions.
#undef setlocale
#undef localeconv

namespace std
{
  using ::lconv;
  using ::setlocale;
  using ::localeconv;
} // namespace std


#define _GLIBCXX_C_LOCALE_GNU 1

#define _GLIBCXX_NUM_CATEGORIES 6


namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  extern "C" __typeof(uselocale) __uselocale;

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace


namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  typedef __locale_t		__c_locale;

  // Convert numeric value of type double and long double to string and
  // return length of string.  If vsnprintf is available use it, otherwise
  // fall back to the unsafe vsprintf which, in general, can be dangerous
  // and should be avoided.
  inline int
  __convert_from_v(const __c_locale& __cloc __attribute__ ((__unused__)),
		   char* __out,
		   const int __size __attribute__ ((__unused__)),
		   const char* __fmt, ...)
  {

    __c_locale __old = __gnu_cxx::__uselocale(__cloc);

    __builtin_va_list __args;
    __builtin_va_start(__args, __fmt);


    const int __ret = __builtin_vsnprintf(__out, __size, __fmt, __args);




    __builtin_va_end(__args);


    __gnu_cxx::__uselocale(__old);







    return __ret;
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace


// -*- C++ -*- forwarding header.

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/cctype
 *  This is a Standard C++ Library file.  You should @c \#include this file
 *  in your programs, rather than any of the @a *.h implementation files.
 *
 *  This is the C++ version of the Standard C Library header @c ctype.h,
 *  and its contents are (mostly) the same as that header, but are all
 *  contained in the namespace @c std (except for names which are defined
 *  as macros in C).
 */

//
// ISO C++ 14882: <ccytpe>
//

       


/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99 Standard 7.4: Character handling	<ctype.h>
 */


#define _CTYPE_H 1




__BEGIN_DECLS


/* These are all the characteristics of characters.
   If there get to be more than 16 distinct characteristics,
   many things must be changed that use `unsigned short int's.

   The characteristics are stored always in network byte order (big
   endian).  We define the bit value interpretations here dependent on the
   machine's byte order.  */

/* Copyright (C) 1992-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _ENDIAN_H 1



/* Definitions for byte order, according to significance of bytes,
   from low addresses to high addresses.  The value is what you get by
   putting '4' in the most significant byte, '3' in the second most
   significant byte, '2' in the second least significant byte, and '1'
   in the least significant byte, and then writing down one digit for
   each byte, starting with the byte at the lowest address at the left,
   and proceeding to the byte with the highest address at the right.  */

#define __LITTLE_ENDIAN 1234
#define __BIG_ENDIAN 4321
#define __PDP_ENDIAN 3412

/* This file defines `__BYTE_ORDER' for the particular machine.  */
/* i386/x86_64 are little-endian.  */





#define __BYTE_ORDER __LITTLE_ENDIAN

/* Some machines may need to use a different endianness for floating point
   values.  */

#define __FLOAT_WORD_ORDER __BYTE_ORDER



#define LITTLE_ENDIAN __LITTLE_ENDIAN
#define BIG_ENDIAN __BIG_ENDIAN
#define PDP_ENDIAN __PDP_ENDIAN
#define BYTE_ORDER __BYTE_ORDER



#define __LONG_LONG_PAIR(HI,LO) LO, HI






/* Conversion interfaces.  */
/* Macros and inline functions to swap the order of bytes in integer values.
   Copyright (C) 1997-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */






#define _BITS_BYTESWAP_H 1




/* Swap bytes in 16-bit value.  */
#define __bswap_constant_16(x) ((__uint16_t) ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8)))


static __inline __uint16_t
__bswap_16 (__uint16_t __bsx)
{

  return __builtin_bswap16 (__bsx);



}

/* Swap bytes in 32-bit value.  */
#define __bswap_constant_32(x) ((((x) & 0xff000000u) >> 24) | (((x) & 0x00ff0000u) >> 8) | (((x) & 0x0000ff00u) << 8) | (((x) & 0x000000ffu) << 24))



static __inline __uint32_t
__bswap_32 (__uint32_t __bsx)
{

  return __builtin_bswap32 (__bsx);



}

/* Swap bytes in 64-bit value.  */
#define __bswap_constant_64(x) ((((x) & 0xff00000000000000ull) >> 56) | (((x) & 0x00ff000000000000ull) >> 40) | (((x) & 0x0000ff0000000000ull) >> 24) | (((x) & 0x000000ff00000000ull) >> 8) | (((x) & 0x00000000ff000000ull) << 8) | (((x) & 0x0000000000ff0000ull) << 24) | (((x) & 0x000000000000ff00ull) << 40) | (((x) & 0x00000000000000ffull) << 56))

__extension__ static __inline __uint64_t
__bswap_64 (__uint64_t __bsx)
{

  return __builtin_bswap64 (__bsx);



}

/* Inline functions to return unsigned integer values unchanged.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */






#define _BITS_UINTN_IDENTITY_H 1



/* These inline functions are to ensure the appropriate type
   conversions and associated diagnostics from macros that convert to
   a given endianness.  */

static __inline __uint16_t
__uint16_identity (__uint16_t __x)
{
  return __x;
}

static __inline __uint32_t
__uint32_identity (__uint32_t __x)
{
  return __x;
}

static __inline __uint64_t
__uint64_identity (__uint64_t __x)
{
  return __x;
}



#define htobe16(x) __bswap_16 (x)
#define htole16(x) __uint16_identity (x)
#define be16toh(x) __bswap_16 (x)
#define le16toh(x) __uint16_identity (x)

#define htobe32(x) __bswap_32 (x)
#define htole32(x) __uint32_identity (x)
#define be32toh(x) __bswap_32 (x)
#define le32toh(x) __uint32_identity (x)

#define htobe64(x) __bswap_64 (x)
#define htole64(x) __uint64_identity (x)
#define be64toh(x) __bswap_64 (x)
#define le64toh(x) __uint64_identity (x)






#define _ISbit(bit) ((bit) < 8 ? ((1 << (bit)) << 8) : ((1 << (bit)) >> 8))


enum
{
  _ISupper = _ISbit (0),	/* UPPERCASE.  */
  _ISlower = _ISbit (1),	/* lowercase.  */
  _ISalpha = _ISbit (2),	/* Alphabetic.  */
  _ISdigit = _ISbit (3),	/* Numeric.  */
  _ISxdigit = _ISbit (4),	/* Hexadecimal numeric.  */
  _ISspace = _ISbit (5),	/* Whitespace.  */
  _ISprint = _ISbit (6),	/* Printing.  */
  _ISgraph = _ISbit (7),	/* Graphical.  */
  _ISblank = _ISbit (8),	/* Blank (usually SPC and TAB).  */
  _IScntrl = _ISbit (9),	/* Control character.  */
  _ISpunct = _ISbit (10),	/* Punctuation.  */
  _ISalnum = _ISbit (11)	/* Alphanumeric.  */
};


/* These are defined in ctype-info.c.
   The declarations here must match those in localeinfo.h.

   In the thread-specific locale model (see `uselocale' in <locale.h>)
   we cannot use global variables for these as was done in the past.
   Instead, the following accessor functions return the address of
   each variable, which is local to the current thread if multithreaded.

   These point into arrays of 384, so they can be indexed by any `unsigned
   char' value [0,255]; by EOF (-1); or by any `signed char' value
   [-128,-1).  ISO C requires that the ctype functions work for `unsigned
   char' values and for EOF; we also support negative `signed char' values
   for broken old programs.  The case conversion arrays are of `int's
   rather than `unsigned char's because tolower (EOF) must be EOF, which
   doesn't fit into an `unsigned char'.  But today more important is that
   the arrays are also used for multi-byte character sets.  */
extern const unsigned short int **__ctype_b_loc (void)
     __THROW __attribute__ ((__const__));
extern const __int32_t **__ctype_tolower_loc (void)
     __THROW __attribute__ ((__const__));
extern const __int32_t **__ctype_toupper_loc (void)
     __THROW __attribute__ ((__const__));



#define __isascii(c) (((c) & ~0x7f) == 0)
#define __toascii(c) ((c) & 0x7f)

#define __exctype(name) extern int name (int) __THROW

/* The following names are all functions:
     int isCHARACTERISTIC(int c);
   which return nonzero iff C has CHARACTERISTIC.
   For the meaning of the characteristic names, see the `enum' above.  */
__exctype (isalnum);
__exctype (isalpha);
__exctype (iscntrl);
__exctype (isdigit);
__exctype (islower);
__exctype (isgraph);
__exctype (isprint);
__exctype (ispunct);
__exctype (isspace);
__exctype (isupper);
__exctype (isxdigit);


/* Return the lowercase version of C.  */
extern int tolower (int __c) __THROW;

/* Return the uppercase version of C.  */
extern int toupper (int __c) __THROW;


/* ISO C99 introduced one new function.  */

__exctype (isblank);



/* Test C for a set of character classes according to MASK.  */
extern int isctype (int __c, int __mask) __THROW;




/* Return nonzero iff C is in the ASCII set
   (i.e., is no more than 7 bits wide).  */
extern int isascii (int __c) __THROW;

/* Return the part of C that is in the ASCII set
   (i.e., the low-order 7 bits of C).  */
extern int toascii (int __c) __THROW;

/* These are the same as `toupper' and `tolower' except that they do not
   check the argument for being in the range of a `char'.  */
__exctype (_toupper);
__exctype (_tolower);


/* This code is needed for the optimized mapping functions.  */
#define __tobody(c,f,a,args) (__extension__ ({ int __res; if (sizeof (c) > 1) { if (__builtin_constant_p (c)) { int __c = (c); __res = __c < -128 || __c > 255 ? __c : (a)[__c]; } else __res = f args; } else __res = (a)[(int) (c)]; __res; }))




/* POSIX.1-2008 extended locale interface (see locale.h).  */


/* These definitions are similar to the ones above but all functions
   take as an argument a handle for the locale which shall be used.  */
#define __isctype_l(c,type,locale) ((locale)->__ctype_b[(int) (c)] & (unsigned short int) type)


#define __exctype_l(name) extern int name (int, locale_t) __THROW


/* The following names are all functions:
     int isCHARACTERISTIC(int c, locale_t *locale);
   which return nonzero iff C has CHARACTERISTIC.
   For the meaning of the characteristic names, see the `enum' above.  */
__exctype_l (isalnum_l);
__exctype_l (isalpha_l);
__exctype_l (iscntrl_l);
__exctype_l (isdigit_l);
__exctype_l (islower_l);
__exctype_l (isgraph_l);
__exctype_l (isprint_l);
__exctype_l (ispunct_l);
__exctype_l (isspace_l);
__exctype_l (isupper_l);
__exctype_l (isxdigit_l);

__exctype_l (isblank_l);


/* Return the lowercase version of C in locale L.  */
extern int __tolower_l (int __c, locale_t __l) __THROW;
extern int tolower_l (int __c, locale_t __l) __THROW;

/* Return the uppercase version of C.  */
extern int __toupper_l (int __c, locale_t __l) __THROW;
extern int toupper_l (int __c, locale_t __l) __THROW;






__END_DECLS



#define _GLIBCXX_CCTYPE 1

// Get rid of those macros defined in <ctype.h> in lieu of real functions.
#undef isalnum
#undef isalpha
#undef iscntrl
#undef isdigit
#undef isgraph
#undef islower
#undef isprint
#undef ispunct
#undef isspace
#undef isupper
#undef isxdigit
#undef tolower
#undef toupper

namespace std
{
  using ::isalnum;
  using ::isalpha;
  using ::iscntrl;
  using ::isdigit;
  using ::isgraph;
  using ::islower;
  using ::isprint;
  using ::ispunct;
  using ::isspace;
  using ::isupper;
  using ::isxdigit;
  using ::tolower;
  using ::toupper;
} // namespace std





#undef isblank

namespace std
{
  using ::isblank;
} // namespace std






namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /** 
   *  @defgroup locales Locales
   *
   *  Classes and functions for internationalization and localization.
   */

  // 22.1.1 Locale
  class locale;

  template<typename _Facet>
    bool
    has_facet(const locale&) throw();

  template<typename _Facet>
    const _Facet&
    use_facet(const locale&);

  // 22.1.3 Convenience interfaces
  template<typename _CharT>
    bool
    isspace(_CharT, const locale&);

  template<typename _CharT>
    bool
    isprint(_CharT, const locale&);

  template<typename _CharT>
    bool
    iscntrl(_CharT, const locale&);

  template<typename _CharT>
    bool
    isupper(_CharT, const locale&);

  template<typename _CharT>
    bool
    islower(_CharT, const locale&);

  template<typename _CharT>
    bool
    isalpha(_CharT, const locale&);

  template<typename _CharT>
    bool
    isdigit(_CharT, const locale&);

  template<typename _CharT>
    bool
    ispunct(_CharT, const locale&);

  template<typename _CharT>
    bool
    isxdigit(_CharT, const locale&);

  template<typename _CharT>
    bool
    isalnum(_CharT, const locale&);

  template<typename _CharT>
    bool
    isgraph(_CharT, const locale&);


  template<typename _CharT>
    bool
    isblank(_CharT, const locale&);


  template<typename _CharT>
    _CharT
    toupper(_CharT, const locale&);

  template<typename _CharT>
    _CharT
    tolower(_CharT, const locale&);

  // 22.2.1 and 22.2.1.3 ctype
  struct ctype_base;
  template<typename _CharT>
    class ctype;
  template<> class ctype<char>;

  template<> class ctype<wchar_t>;

  template<typename _CharT>
    class ctype_byname;
  // NB: Specialized for char and wchar_t in locale_facets.h.

  class codecvt_base;
  template<typename _InternT, typename _ExternT, typename _StateT>
    class codecvt;
  template<> class codecvt<char, char, mbstate_t>;

  template<> class codecvt<wchar_t, char, mbstate_t>;


  template<> class codecvt<char16_t, char, mbstate_t>;
  template<> class codecvt<char32_t, char, mbstate_t>;





  template<typename _InternT, typename _ExternT, typename _StateT>
    class codecvt_byname;

  // 22.2.2 and 22.2.3 numeric
_GLIBCXX_BEGIN_NAMESPACE_LDBL
  template<typename _CharT, typename _InIter = istreambuf_iterator<_CharT> >
    class num_get;
  template<typename _CharT, typename _OutIter = ostreambuf_iterator<_CharT> >
    class num_put;
_GLIBCXX_END_NAMESPACE_LDBL
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template<typename _CharT> class numpunct;
  template<typename _CharT> class numpunct_byname;
_GLIBCXX_END_NAMESPACE_CXX11

_GLIBCXX_BEGIN_NAMESPACE_CXX11
  // 22.2.4 collation
  template<typename _CharT>
    class collate;
  template<typename _CharT>
    class collate_byname;
_GLIBCXX_END_NAMESPACE_CXX11

  // 22.2.5 date and time
  class time_base;
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template<typename _CharT, typename _InIter =  istreambuf_iterator<_CharT> >
    class time_get;
  template<typename _CharT, typename _InIter =  istreambuf_iterator<_CharT> >
    class time_get_byname;
_GLIBCXX_END_NAMESPACE_CXX11
  template<typename _CharT, typename _OutIter = ostreambuf_iterator<_CharT> >
    class time_put;
  template<typename _CharT, typename _OutIter = ostreambuf_iterator<_CharT> >
    class time_put_byname;

  // 22.2.6 money
  class money_base;
_GLIBCXX_BEGIN_NAMESPACE_LDBL_OR_CXX11
  template<typename _CharT, typename _InIter =  istreambuf_iterator<_CharT> >
    class money_get;
  template<typename _CharT, typename _OutIter = ostreambuf_iterator<_CharT> >
    class money_put;
_GLIBCXX_END_NAMESPACE_LDBL_OR_CXX11
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template<typename _CharT, bool _Intl = false>
    class moneypunct;
  template<typename _CharT, bool _Intl = false>
    class moneypunct_byname;
_GLIBCXX_END_NAMESPACE_CXX11

  // 22.2.7 message retrieval
  struct messages_base;
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template<typename _CharT>
    class messages;
  template<typename _CharT>
    class messages_byname;
_GLIBCXX_END_NAMESPACE_CXX11

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

// Iostreams base classes -*- C++ -*-

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/ios_base.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{ios}
 */

//
// ISO C++ 14882: 27.4  Iostreams base classes
//


#define _IOS_BASE_H 1

       

// Support for atomic operations -*- C++ -*-

// Copyright (C) 2004-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file ext/atomicity.h
 *  This file is a GNU extension to the Standard C++ Library.
 */


#define _GLIBCXX_ATOMICITY_H 1

       


/* Threads compatibility routines for libgcc2.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1997-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#define _GLIBCXX_GCC_GTHR_H 


#pragma GCC visibility push(default)


/* If this file is compiled with threads support, it must
       #define __GTHREADS 1
   to indicate that threads support is present.  Also it has define
   function
     int __gthread_active_p ()
   that returns 1 if thread system is active, 0 if not.

   The threads interface must define the following types:
     __gthread_key_t
     __gthread_once_t
     __gthread_mutex_t
     __gthread_recursive_mutex_t

   The threads interface must define the following macros:

     __GTHREAD_ONCE_INIT
     		to initialize __gthread_once_t
     __GTHREAD_MUTEX_INIT
     		to initialize __gthread_mutex_t to get a fast
		non-recursive mutex.
     __GTHREAD_MUTEX_INIT_FUNCTION
		to initialize __gthread_mutex_t to get a fast
		non-recursive mutex.
		Define this to a function which looks like this:
		  void __GTHREAD_MUTEX_INIT_FUNCTION (__gthread_mutex_t *)
     		Some systems can't initialize a mutex without a
		function call.  Don't define __GTHREAD_MUTEX_INIT in this case.
     __GTHREAD_RECURSIVE_MUTEX_INIT
     __GTHREAD_RECURSIVE_MUTEX_INIT_FUNCTION
     		as above, but for a recursive mutex.

   The threads interface must define the following static functions:

     int __gthread_once (__gthread_once_t *once, void (*func) ())

     int __gthread_key_create (__gthread_key_t *keyp, void (*dtor) (void *))
     int __gthread_key_delete (__gthread_key_t key)

     void *__gthread_getspecific (__gthread_key_t key)
     int __gthread_setspecific (__gthread_key_t key, const void *ptr)

     int __gthread_mutex_destroy (__gthread_mutex_t *mutex);
     int __gthread_recursive_mutex_destroy (__gthread_recursive_mutex_t *mutex);

     int __gthread_mutex_lock (__gthread_mutex_t *mutex);
     int __gthread_mutex_trylock (__gthread_mutex_t *mutex);
     int __gthread_mutex_unlock (__gthread_mutex_t *mutex);

     int __gthread_recursive_mutex_lock (__gthread_recursive_mutex_t *mutex);
     int __gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t *mutex);
     int __gthread_recursive_mutex_unlock (__gthread_recursive_mutex_t *mutex);

   The following are supported in POSIX threads only. They are required to
   fix a deadlock in static initialization inside libsupc++. The header file
   gthr-posix.h defines a symbol __GTHREAD_HAS_COND to signify that these extra
   features are supported.

   Types:
     __gthread_cond_t

   Macros:
     __GTHREAD_COND_INIT
     __GTHREAD_COND_INIT_FUNCTION

   Interface:
     int __gthread_cond_broadcast (__gthread_cond_t *cond);
     int __gthread_cond_wait (__gthread_cond_t *cond, __gthread_mutex_t *mutex);
     int __gthread_cond_wait_recursive (__gthread_cond_t *cond,
					__gthread_recursive_mutex_t *mutex);

   All functions returning int should return zero on success or the error
   number.  If the operation is not supported, -1 is returned.

   If the following are also defined, you should
     #define __GTHREADS_CXX0X 1
   to enable the c++0x thread library.

   Types:
     __gthread_t
     __gthread_time_t

   Interface:
     int __gthread_create (__gthread_t *thread, void *(*func) (void*),
                           void *args);
     int __gthread_join (__gthread_t thread, void **value_ptr);
     int __gthread_detach (__gthread_t thread);
     int __gthread_equal (__gthread_t t1, __gthread_t t2);
     __gthread_t __gthread_self (void);
     int __gthread_yield (void);

     int __gthread_mutex_timedlock (__gthread_mutex_t *m,
                                    const __gthread_time_t *abs_timeout);
     int __gthread_recursive_mutex_timedlock (__gthread_recursive_mutex_t *m,
                                          const __gthread_time_t *abs_time);

     int __gthread_cond_signal (__gthread_cond_t *cond);
     int __gthread_cond_timedwait (__gthread_cond_t *cond,
                                   __gthread_mutex_t *mutex,
                                   const __gthread_time_t *abs_timeout);

*/


/* The pe-coff weak support isn't fully compatible to ELF's weak.
   For static libraries it might would work, but as we need to deal
   with shared versions too, we disable it for mingw-targets.  */






#define _GLIBCXX_GTHREAD_USE_WEAK 1


/* Threads compatibility routines for libgcc2 and libobjc.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1997-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#define _GLIBCXX_GCC_GTHR_POSIX_H 

/* POSIX threads specific definitions.
   Easy, since the interface is just one-to-one mapping.  */

#define __GTHREADS 1
#define __GTHREADS_CXX0X 1

/* Copyright (C) 2002-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _PTHREAD_H 1



/* Definitions for POSIX 1003.1b-1993 (aka POSIX.4) scheduling interface.
   Copyright (C) 1996-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _SCHED_H 1



/* Get type definitions.  */


#define __need_size_t 
#define __need_NULL 
/* Copyright (C) 1989-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */






/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */


/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */










/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */




/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */













/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */

/* Signed type of difference of two pointers.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


/* If this symbol has done its job, get rid of it.  */
#undef __need_ptrdiff_t



/* Unsigned type of `sizeof' something.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

#undef __need_size_t



/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

#undef __need_wchar_t






/* A null pointer constant.  */


#undef NULL

#define NULL __null








#undef __need_NULL



/* Offset of member MEMBER in a struct of type TYPE. */
#define offsetof(TYPE,MEMBER) __builtin_offsetof (TYPE, MEMBER)
















#define __time_t_defined 1



/* Returned by `time'.  */
typedef __time_t time_t;

/* NB: Include guard matches what <linux/time.h> uses.  */

#define _STRUCT_TIMESPEC 1



/* POSIX.1b structure for a time value.  This is like a `struct timeval' but
   has nanoseconds instead of microseconds.  */
struct timespec
{
  __time_t tv_sec;		/* Seconds.  */
  __syscall_slong_t tv_nsec;	/* Nanoseconds.  */
};






typedef __pid_t pid_t;
#define __pid_t_defined 


/* Get system specific constant and data structure definitions.  */
/* Definitions of constants and data structure for POSIX 1003.1b-1993
   scheduling interface.
   Copyright (C) 1996-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_SCHED_H 1





/* Scheduling algorithms.  */
#define SCHED_OTHER 0
#define SCHED_FIFO 1
#define SCHED_RR 2

#define SCHED_BATCH 3
#define SCHED_ISO 4
#define SCHED_IDLE 5
#define SCHED_DEADLINE 6

#define SCHED_RESET_ON_FORK 0x40000000



/* Cloning flags.  */
#define CSIGNAL 0x000000ff
#define CLONE_VM 0x00000100
#define CLONE_FS 0x00000200
#define CLONE_FILES 0x00000400
#define CLONE_SIGHAND 0x00000800
#define CLONE_PTRACE 0x00002000
#define CLONE_VFORK 0x00004000

#define CLONE_PARENT 0x00008000

#define CLONE_THREAD 0x00010000
#define CLONE_NEWNS 0x00020000
#define CLONE_SYSVSEM 0x00040000
#define CLONE_SETTLS 0x00080000
#define CLONE_PARENT_SETTID 0x00100000

#define CLONE_CHILD_CLEARTID 0x00200000

#define CLONE_DETACHED 0x00400000
#define CLONE_UNTRACED 0x00800000

#define CLONE_CHILD_SETTID 0x01000000

#define CLONE_NEWCGROUP 0x02000000
#define CLONE_NEWUTS 0x04000000
#define CLONE_NEWIPC 0x08000000
#define CLONE_NEWUSER 0x10000000
#define CLONE_NEWPID 0x20000000
#define CLONE_NEWNET 0x40000000
#define CLONE_IO 0x80000000


/* Sched parameter structure.  Generic version.
   Copyright (C) 1996-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library;  if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_TYPES_STRUCT_SCHED_PARAM 1

/* Data structure to describe a process' schedulability.  */
struct sched_param
{
  int sched_priority;
};


__BEGIN_DECLS


/* Clone current process.  */
extern int clone (int (*__fn) (void *__arg), void *__child_stack,
		  int __flags, void *__arg, ...) __THROW;

/* Unshare the specified resources.  */
extern int unshare (int __flags) __THROW;

/* Get index of currently used CPU.  */
extern int sched_getcpu (void) __THROW;

/* Switch process to namespace of type NSTYPE indicated by FD.  */
extern int setns (int __fd, int __nstype) __THROW;


__END_DECLS

/* Definition of the cpu_set_t structure used by the POSIX 1003.1b-1993
   scheduling interface.
   Copyright (C) 1996-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_CPU_SET_H 1





/* Size definition for CPU sets.  */
#define __CPU_SETSIZE 1024
#define __NCPUBITS (8 * sizeof (__cpu_mask))

/* Type for array elements in 'cpu_set_t'.  */
typedef __CPU_MASK_TYPE __cpu_mask;

/* Basic access functions.  */
#define __CPUELT(cpu) ((cpu) / __NCPUBITS)
#define __CPUMASK(cpu) ((__cpu_mask) 1 << ((cpu) % __NCPUBITS))

/* Data structure to describe CPU mask.  */
typedef struct
{
  __cpu_mask __bits[__CPU_SETSIZE / __NCPUBITS];
} cpu_set_t;

/* Access functions for CPU masks.  */

#define __CPU_ZERO_S(setsize,cpusetp) do __builtin_memset (cpusetp, '\0', setsize); while (0)

#define __CPU_SET_S(cpu,setsize,cpusetp) (__extension__ ({ size_t __cpu = (cpu); __cpu / 8 < (setsize) ? (((__cpu_mask *) ((cpusetp)->__bits))[__CPUELT (__cpu)] |= __CPUMASK (__cpu)) : 0; }))






#define __CPU_CLR_S(cpu,setsize,cpusetp) (__extension__ ({ size_t __cpu = (cpu); __cpu / 8 < (setsize) ? (((__cpu_mask *) ((cpusetp)->__bits))[__CPUELT (__cpu)] &= ~__CPUMASK (__cpu)) : 0; }))






#define __CPU_ISSET_S(cpu,setsize,cpusetp) (__extension__ ({ size_t __cpu = (cpu); __cpu / 8 < (setsize) ? ((((const __cpu_mask *) ((cpusetp)->__bits))[__CPUELT (__cpu)] & __CPUMASK (__cpu))) != 0 : 0; }))







#define __CPU_COUNT_S(setsize,cpusetp) __sched_cpucount (setsize, cpusetp)



#define __CPU_EQUAL_S(setsize,cpusetp1,cpusetp2) (__builtin_memcmp (cpusetp1, cpusetp2, setsize) == 0)


#define __CPU_OP_S(setsize,destset,srcset1,srcset2,op) (__extension__ ({ cpu_set_t *__dest = (destset); const __cpu_mask *__arr1 = (srcset1)->__bits; const __cpu_mask *__arr2 = (srcset2)->__bits; size_t __imax = (setsize) / sizeof (__cpu_mask); size_t __i; for (__i = 0; __i < __imax; ++__i) ((__cpu_mask *) __dest->__bits)[__i] = __arr1[__i] op __arr2[__i]; __dest; }))

#define __CPU_ALLOC_SIZE(count) ((((count) + __NCPUBITS - 1) / __NCPUBITS) * sizeof (__cpu_mask))

#define __CPU_ALLOC(count) __sched_cpualloc (count)
#define __CPU_FREE(cpuset) __sched_cpufree (cpuset)

__BEGIN_DECLS

extern int __sched_cpucount (size_t __setsize, const cpu_set_t *__setp)
     __THROW;
extern cpu_set_t *__sched_cpualloc (size_t __count) __THROW __wur;
extern void __sched_cpufree (cpu_set_t *__set) __THROW;

__END_DECLS


/* Backward compatibility.  */
#define sched_priority sched_priority
#define __sched_priority sched_priority


__BEGIN_DECLS

/* Set scheduling parameters for a process.  */
extern int sched_setparam (__pid_t __pid, const struct sched_param *__param)
     __THROW;

/* Retrieve scheduling parameters for a particular process.  */
extern int sched_getparam (__pid_t __pid, struct sched_param *__param) __THROW;

/* Set scheduling algorithm and/or parameters for a process.  */
extern int sched_setscheduler (__pid_t __pid, int __policy,
			       const struct sched_param *__param) __THROW;

/* Retrieve scheduling algorithm for a particular purpose.  */
extern int sched_getscheduler (__pid_t __pid) __THROW;

/* Yield the processor.  */
extern int sched_yield (void) __THROW;

/* Get maximum priority value for a scheduler.  */
extern int sched_get_priority_max (int __algorithm) __THROW;

/* Get minimum priority value for a scheduler.  */
extern int sched_get_priority_min (int __algorithm) __THROW;

/* Get the SCHED_RR interval for the named process.  */
extern int sched_rr_get_interval (__pid_t __pid, struct timespec *__t) __THROW;



/* Access macros for `cpu_set'.  */
#define CPU_SETSIZE __CPU_SETSIZE
#define CPU_SET(cpu,cpusetp) __CPU_SET_S (cpu, sizeof (cpu_set_t), cpusetp)
#define CPU_CLR(cpu,cpusetp) __CPU_CLR_S (cpu, sizeof (cpu_set_t), cpusetp)
#define CPU_ISSET(cpu,cpusetp) __CPU_ISSET_S (cpu, sizeof (cpu_set_t), cpusetp)

#define CPU_ZERO(cpusetp) __CPU_ZERO_S (sizeof (cpu_set_t), cpusetp)
#define CPU_COUNT(cpusetp) __CPU_COUNT_S (sizeof (cpu_set_t), cpusetp)

#define CPU_SET_S(cpu,setsize,cpusetp) __CPU_SET_S (cpu, setsize, cpusetp)
#define CPU_CLR_S(cpu,setsize,cpusetp) __CPU_CLR_S (cpu, setsize, cpusetp)
#define CPU_ISSET_S(cpu,setsize,cpusetp) __CPU_ISSET_S (cpu, setsize, cpusetp)

#define CPU_ZERO_S(setsize,cpusetp) __CPU_ZERO_S (setsize, cpusetp)
#define CPU_COUNT_S(setsize,cpusetp) __CPU_COUNT_S (setsize, cpusetp)

#define CPU_EQUAL(cpusetp1,cpusetp2) __CPU_EQUAL_S (sizeof (cpu_set_t), cpusetp1, cpusetp2)

#define CPU_EQUAL_S(setsize,cpusetp1,cpusetp2) __CPU_EQUAL_S (setsize, cpusetp1, cpusetp2)


#define CPU_AND(destset,srcset1,srcset2) __CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, &)

#define CPU_OR(destset,srcset1,srcset2) __CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, |)

#define CPU_XOR(destset,srcset1,srcset2) __CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, ^)

#define CPU_AND_S(setsize,destset,srcset1,srcset2) __CPU_OP_S (setsize, destset, srcset1, srcset2, &)

#define CPU_OR_S(setsize,destset,srcset1,srcset2) __CPU_OP_S (setsize, destset, srcset1, srcset2, |)

#define CPU_XOR_S(setsize,destset,srcset1,srcset2) __CPU_OP_S (setsize, destset, srcset1, srcset2, ^)


#define CPU_ALLOC_SIZE(count) __CPU_ALLOC_SIZE (count)
#define CPU_ALLOC(count) __CPU_ALLOC (count)
#define CPU_FREE(cpuset) __CPU_FREE (cpuset)


/* Set the CPU affinity for a task */
extern int sched_setaffinity (__pid_t __pid, size_t __cpusetsize,
			      const cpu_set_t *__cpuset) __THROW;

/* Get the CPU affinity for a task */
extern int sched_getaffinity (__pid_t __pid, size_t __cpusetsize,
			      cpu_set_t *__cpuset) __THROW;


__END_DECLS

/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99 Standard: 7.23 Date and time	<time.h>
 */


#define _TIME_H 1



#define __need_size_t 
#define __need_NULL 
/* Copyright (C) 1989-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */






/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */


/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */










/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */




/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */













/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */

/* Signed type of difference of two pointers.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


/* If this symbol has done its job, get rid of it.  */
#undef __need_ptrdiff_t



/* Unsigned type of `sizeof' something.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

#undef __need_size_t



/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

#undef __need_wchar_t






/* A null pointer constant.  */


#undef NULL

#define NULL __null








#undef __need_NULL



/* Offset of member MEMBER in a struct of type TYPE. */
#define offsetof(TYPE,MEMBER) __builtin_offsetof (TYPE, MEMBER)















/* This defines CLOCKS_PER_SEC, which is the number of processor clock
   ticks per second, and possibly a number of other constants.   */
/* System-dependent timing definitions.  Linux version.
   Copyright (C) 1996-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <time.h> instead.
 */


#define _BITS_TIME_H 1



/* ISO/IEC 9899:1999 7.23.1: Components of time
   The macro `CLOCKS_PER_SEC' is an expression with type `clock_t' that is
   the number per second of the value returned by the `clock' function.  */
/* CAE XSH, Issue 4, Version 2: <time.h>
   The value of CLOCKS_PER_SEC is required to be 1 million on all
   XSI-conformant systems. */
#define CLOCKS_PER_SEC ((__clock_t) 1000000)










/* Identifier for system-wide realtime clock.  */
#define CLOCK_REALTIME 0
/* Monotonic system-wide clock.  */
#define CLOCK_MONOTONIC 1
/* High-resolution timer from the CPU.  */
#define CLOCK_PROCESS_CPUTIME_ID 2
/* Thread-specific CPU-time clock.  */
#define CLOCK_THREAD_CPUTIME_ID 3
/* Monotonic system-wide clock, not adjusted for frequency scaling.  */
#define CLOCK_MONOTONIC_RAW 4
/* Identifier for system-wide realtime clock, updated only on ticks.  */
#define CLOCK_REALTIME_COARSE 5
/* Monotonic system-wide clock, updated only on ticks.  */
#define CLOCK_MONOTONIC_COARSE 6
/* Monotonic system-wide clock that includes time spent in suspension.  */
#define CLOCK_BOOTTIME 7
/* Like CLOCK_REALTIME but also wakes suspended system.  */
#define CLOCK_REALTIME_ALARM 8
/* Like CLOCK_BOOTTIME but also wakes suspended system.  */
#define CLOCK_BOOTTIME_ALARM 9
/* Like CLOCK_REALTIME but in International Atomic Time.  */
#define CLOCK_TAI 11

/* Flag to indicate time is absolute.  */
#define TIMER_ABSTIME 1



/* Copyright (C) 1995-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_TIMEX_H 1



#define __timeval_defined 1



/* A time value that is accurate to the nearest
   microsecond but also has a range of years.  */
struct timeval
{
  __time_t tv_sec;		/* Seconds.  */
  __suseconds_t tv_usec;	/* Microseconds.  */
};

/* These definitions from linux/timex.h as of 3.18.  */

struct timex
{
  unsigned int modes;		/* mode selector */
  __syscall_slong_t offset;	/* time offset (usec) */
  __syscall_slong_t freq;	/* frequency offset (scaled ppm) */
  __syscall_slong_t maxerror;	/* maximum error (usec) */
  __syscall_slong_t esterror;	/* estimated error (usec) */
  int status;			/* clock command/status */
  __syscall_slong_t constant;	/* pll time constant */
  __syscall_slong_t precision;	/* clock precision (usec) (ro) */
  __syscall_slong_t tolerance;	/* clock frequency tolerance (ppm) (ro) */
  struct timeval time;		/* (read only, except for ADJ_SETOFFSET) */
  __syscall_slong_t tick;	/* (modified) usecs between clock ticks */
  __syscall_slong_t ppsfreq;	/* pps frequency (scaled ppm) (ro) */
  __syscall_slong_t jitter;	/* pps jitter (us) (ro) */
  int shift;			/* interval duration (s) (shift) (ro) */
  __syscall_slong_t stabil;	/* pps stability (scaled ppm) (ro) */
  __syscall_slong_t jitcnt;	/* jitter limit exceeded (ro) */
  __syscall_slong_t calcnt;	/* calibration intervals (ro) */
  __syscall_slong_t errcnt;	/* calibration errors (ro) */
  __syscall_slong_t stbcnt;	/* stability limit exceeded (ro) */

  int tai;			/* TAI offset (ro) */

  /* ??? */
  int  :32; int  :32; int  :32; int  :32;
  int  :32; int  :32; int  :32; int  :32;
  int  :32; int  :32; int  :32;
};

/* Mode codes (timex.mode) */
#define ADJ_OFFSET 0x0001
#define ADJ_FREQUENCY 0x0002
#define ADJ_MAXERROR 0x0004
#define ADJ_ESTERROR 0x0008
#define ADJ_STATUS 0x0010
#define ADJ_TIMECONST 0x0020
#define ADJ_TAI 0x0080
#define ADJ_SETOFFSET 0x0100
#define ADJ_MICRO 0x1000
#define ADJ_NANO 0x2000
#define ADJ_TICK 0x4000
#define ADJ_OFFSET_SINGLESHOT 0x8001
#define ADJ_OFFSET_SS_READ 0xa001

/* xntp 3.4 compatibility names */
#define MOD_OFFSET ADJ_OFFSET
#define MOD_FREQUENCY ADJ_FREQUENCY
#define MOD_MAXERROR ADJ_MAXERROR
#define MOD_ESTERROR ADJ_ESTERROR
#define MOD_STATUS ADJ_STATUS
#define MOD_TIMECONST ADJ_TIMECONST
#define MOD_CLKB ADJ_TICK
#define MOD_CLKA ADJ_OFFSET_SINGLESHOT
#define MOD_TAI ADJ_TAI
#define MOD_MICRO ADJ_MICRO
#define MOD_NANO ADJ_NANO


/* Status codes (timex.status) */
#define STA_PLL 0x0001
#define STA_PPSFREQ 0x0002
#define STA_PPSTIME 0x0004
#define STA_FLL 0x0008

#define STA_INS 0x0010
#define STA_DEL 0x0020
#define STA_UNSYNC 0x0040
#define STA_FREQHOLD 0x0080

#define STA_PPSSIGNAL 0x0100
#define STA_PPSJITTER 0x0200
#define STA_PPSWANDER 0x0400
#define STA_PPSERROR 0x0800

#define STA_CLOCKERR 0x1000
#define STA_NANO 0x2000
#define STA_MODE 0x4000
#define STA_CLK 0x8000

/* Read-only bits */
#define STA_RONLY (STA_PPSSIGNAL | STA_PPSJITTER | STA_PPSWANDER | STA_PPSERROR | STA_CLOCKERR | STA_NANO | STA_MODE | STA_CLK)



__BEGIN_DECLS

/* Tune a POSIX clock.  */
extern int clock_adjtime (__clockid_t __clock_id, struct timex *__utx) __THROW;

__END_DECLS



/* Many of the typedefs and structs whose official home is this header
   may also need to be defined by other headers.  */

#define __clock_t_defined 1



/* Returned by `clock'.  */
typedef __clock_t clock_t;



#define __struct_tm_defined 1



/* ISO C `broken-down time' structure.  */
struct tm
{
  int tm_sec;			/* Seconds.	[0-60] (1 leap second) */
  int tm_min;			/* Minutes.	[0-59] */
  int tm_hour;			/* Hours.	[0-23] */
  int tm_mday;			/* Day.		[1-31] */
  int tm_mon;			/* Month.	[0-11] */
  int tm_year;			/* Year	- 1900.  */
  int tm_wday;			/* Day of week.	[0-6] */
  int tm_yday;			/* Days in year.[0-365]	*/
  int tm_isdst;			/* DST.		[-1/0/1]*/


  long int tm_gmtoff;		/* Seconds east of UTC.  */
  const char *tm_zone;		/* Timezone abbreviation.  */




};








#define __clockid_t_defined 1



/* Clock ID used in clock and timer functions.  */
typedef __clockid_t clockid_t;


#define __timer_t_defined 1



/* Timer ID returned by `timer_create'.  */
typedef __timer_t timer_t;


#define __itimerspec_defined 1




/* POSIX.1b structure for timer start values and intervals.  */
struct itimerspec
  {
    struct timespec it_interval;
    struct timespec it_value;
  };

struct sigevent;














/* Time base values for timespec_get.  */
#define TIME_UTC 1


__BEGIN_DECLS

/* Time used by the program so far (user time + system time).
   The result / CLOCKS_PER_SEC is program time in seconds.  */
extern clock_t clock (void) __THROW;

/* Return the current time and put it in *TIMER if TIMER is not NULL.  */
extern time_t time (time_t *__timer) __THROW;

/* Return the difference between TIME1 and TIME0.  */
extern double difftime (time_t __time1, time_t __time0)
     __THROW __attribute__ ((__const__));

/* Return the `time_t' representation of TP and normalize TP.  */
extern time_t mktime (struct tm *__tp) __THROW;


/* Format TP into S according to FORMAT.
   Write no more than MAXSIZE characters and return the number
   of characters written, or 0 if it would exceed MAXSIZE.  */
extern size_t strftime (char *__restrict __s, size_t __maxsize,
			const char *__restrict __format,
			const struct tm *__restrict __tp) __THROW;


/* Parse S according to FORMAT and store binary time information in TP.
   The return value is a pointer to the first unparsed character in S.  */
extern char *strptime (const char *__restrict __s,
		       const char *__restrict __fmt, struct tm *__tp)
     __THROW;



/* Similar to the two functions above but take the information from
   the provided locale and not the global locale.  */

extern size_t strftime_l (char *__restrict __s, size_t __maxsize,
			  const char *__restrict __format,
			  const struct tm *__restrict __tp,
			  locale_t __loc) __THROW;



extern char *strptime_l (const char *__restrict __s,
			 const char *__restrict __fmt, struct tm *__tp,
			 locale_t __loc) __THROW;



/* Return the `struct tm' representation of *TIMER
   in Universal Coordinated Time (aka Greenwich Mean Time).  */
extern struct tm *gmtime (const time_t *__timer) __THROW;

/* Return the `struct tm' representation
   of *TIMER in the local timezone.  */
extern struct tm *localtime (const time_t *__timer) __THROW;


/* Return the `struct tm' representation of *TIMER in UTC,
   using *TP to store the result.  */
extern struct tm *gmtime_r (const time_t *__restrict __timer,
			    struct tm *__restrict __tp) __THROW;

/* Return the `struct tm' representation of *TIMER in local time,
   using *TP to store the result.  */
extern struct tm *localtime_r (const time_t *__restrict __timer,
			       struct tm *__restrict __tp) __THROW;


/* Return a string of the form "Day Mon dd hh:mm:ss yyyy\n"
   that is the representation of TP in this format.  */
extern char *asctime (const struct tm *__tp) __THROW;

/* Equivalent to `asctime (localtime (timer))'.  */
extern char *ctime (const time_t *__timer) __THROW;


/* Reentrant versions of the above functions.  */

/* Return in BUF a string of the form "Day Mon dd hh:mm:ss yyyy\n"
   that is the representation of TP in this format.  */
extern char *asctime_r (const struct tm *__restrict __tp,
			char *__restrict __buf) __THROW;

/* Equivalent to `asctime_r (localtime_r (timer, *TMP*), buf)'.  */
extern char *ctime_r (const time_t *__restrict __timer,
		      char *__restrict __buf) __THROW;



/* Defined in localtime.c.  */
extern char *__tzname[2];	/* Current timezone names.  */
extern int __daylight;		/* If daylight-saving time is ever in use.  */
extern long int __timezone;	/* Seconds west of UTC.  */



/* Same as above.  */
extern char *tzname[2];

/* Set time conversion information from the TZ environment variable.
   If TZ is not defined, a locale-dependent default is used.  */
extern void tzset (void) __THROW;



extern int daylight;
extern long int timezone;



/* Set the system time to *WHEN.
   This call is restricted to the superuser.  */
extern int stime (const time_t *__when) __THROW;



/* Nonzero if YEAR is a leap year (every 4 years,
   except every 100th isn't, and every 400th is).  */
#define __isleap(year) ((year) % 4 == 0 && ((year) % 100 != 0 || (year) % 400 == 0))




/* Miscellaneous functions many Unices inherited from the public domain
   localtime package.  These are included only for compatibility.  */

/* Like `mktime', but for TP represents Universal Time, not local time.  */
extern time_t timegm (struct tm *__tp) __THROW;

/* Another name for `mktime'.  */
extern time_t timelocal (struct tm *__tp) __THROW;

/* Return the number of days in YEAR.  */
extern int dysize (int __year) __THROW  __attribute__ ((__const__));




/* Pause execution for a number of nanoseconds.

   This function is a cancellation point and therefore not marked with
   __THROW.  */
extern int nanosleep (const struct timespec *__requested_time,
		      struct timespec *__remaining);


/* Get resolution of clock CLOCK_ID.  */
extern int clock_getres (clockid_t __clock_id, struct timespec *__res) __THROW;

/* Get current value of clock CLOCK_ID and store it in TP.  */
extern int clock_gettime (clockid_t __clock_id, struct timespec *__tp) __THROW;

/* Set clock CLOCK_ID to value TP.  */
extern int clock_settime (clockid_t __clock_id, const struct timespec *__tp)
     __THROW;


/* High-resolution sleep with the specified clock.

   This function is a cancellation point and therefore not marked with
   __THROW.  */
extern int clock_nanosleep (clockid_t __clock_id, int __flags,
			    const struct timespec *__req,
			    struct timespec *__rem);

/* Return clock ID for CPU-time clock.  */
extern int clock_getcpuclockid (pid_t __pid, clockid_t *__clock_id) __THROW;



/* Create new per-process timer using CLOCK_ID.  */
extern int timer_create (clockid_t __clock_id,
			 struct sigevent *__restrict __evp,
			 timer_t *__restrict __timerid) __THROW;

/* Delete timer TIMERID.  */
extern int timer_delete (timer_t __timerid) __THROW;

/* Set timer TIMERID to VALUE, returning old value in OVALUE.  */
extern int timer_settime (timer_t __timerid, int __flags,
			  const struct itimerspec *__restrict __value,
			  struct itimerspec *__restrict __ovalue) __THROW;

/* Get current value of timer TIMERID and store it in VALUE.  */
extern int timer_gettime (timer_t __timerid, struct itimerspec *__value)
     __THROW;

/* Get expiration overrun for timer TIMERID.  */
extern int timer_getoverrun (timer_t __timerid) __THROW;




/* Set TS to calendar time based in time base BASE.  */
extern int timespec_get (struct timespec *__ts, int __base)
     __THROW __nonnull ((1));




/* Set to one of the following values to indicate an error.
     1  the DATEMSK environment variable is null or undefined,
     2  the template file cannot be opened for reading,
     3  failed to get file status information,
     4  the template file is not a regular file,
     5  an error is encountered while reading the template file,
     6  memory allication failed (not enough memory available),
     7  there is no line in the template that matches the input,
     8  invalid input specification Example: February 31 or a time is
	specified that can not be represented in a time_t (representing
	the time in seconds since 00:00:00 UTC, January 1, 1970) */
extern int getdate_err;

/* Parse the given string as a date specification and return a value
   representing the value.  The templates from the file identified by
   the environment variable DATEMSK are used.  In case of an error
   `getdate_err' is set.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern struct tm *getdate (const char *__string);



/* Since `getdate' is not reentrant because of the use of `getdate_err'
   and the static buffer to return the result in, we provide a thread-safe
   variant.  The functionality is the same.  The result is returned in
   the buffer pointed to by RESBUFP and in case of an error the return
   value is != 0 with the same values as given above for `getdate_err'.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */
extern int getdate_r (const char *__restrict __string,
		      struct tm *__restrict __resbufp);


__END_DECLS


/* Declaration of common pthread types for all architectures.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_PTHREADTYPES_COMMON_H 1

/* For internal mutex and condition variable definitions.  */
/* Common threading primitives definitions for both POSIX and C11.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _THREAD_SHARED_TYPES_H 1

/* Arch-specific definitions.  Each architecture must define the following
   macros to define the expected sizes of pthread data types:

   __SIZEOF_PTHREAD_ATTR_T        - size of pthread_attr_t.
   __SIZEOF_PTHREAD_MUTEX_T       - size of pthread_mutex_t.
   __SIZEOF_PTHREAD_MUTEXATTR_T   - size of pthread_mutexattr_t.
   __SIZEOF_PTHREAD_COND_T        - size of pthread_cond_t.
   __SIZEOF_PTHREAD_CONDATTR_T    - size of pthread_condattr_t.
   __SIZEOF_PTHREAD_RWLOCK_T      - size of pthread_rwlock_t.
   __SIZEOF_PTHREAD_RWLOCKATTR_T  - size of pthread_rwlockattr_t.
   __SIZEOF_PTHREAD_BARRIER_T     - size of pthread_barrier_t.
   __SIZEOF_PTHREAD_BARRIERATTR_T - size of pthread_barrierattr_t.

   Also, the following macros must be define for internal pthread_mutex_t
   struct definitions (struct __pthread_mutex_s):

   __PTHREAD_COMPAT_PADDING_MID   - any additional members after 'kind'
				    and before '__spin' (for 64 bits) or
				    '__nusers' (for 32 bits).
   __PTHREAD_COMPAT_PADDING_END   - any additional members at the end of
				    the internal structure.
   __PTHREAD_MUTEX_LOCK_ELISION   - 1 if the architecture supports lock
				    elision or 0 otherwise.
   __PTHREAD_MUTEX_NUSERS_AFTER_KIND - control where to put __nusers.  The
				       preferred value for new architectures
				       is 0.
   __PTHREAD_MUTEX_USE_UNION      - control whether internal __spins and
				    __list will be place inside a union for
				    linuxthreads compatibility.
				    The preferred value for new architectures
				    is 0.

   For a new port the preferred values for the required defines are:

   #define __PTHREAD_COMPAT_PADDING_MID
   #define __PTHREAD_COMPAT_PADDING_END
   #define __PTHREAD_MUTEX_LOCK_ELISION         0
   #define __PTHREAD_MUTEX_NUSERS_AFTER_KIND    0
   #define __PTHREAD_MUTEX_USE_UNION            0

   __PTHREAD_MUTEX_LOCK_ELISION can be set to 1 if the hardware plans to
   eventually support lock elision using transactional memory.

   The additional macro defines any constraint for the lock alignment
   inside the thread structures:

   __LOCK_ALIGNMENT - for internal lock/futex usage.

   Same idea but for the once locking primitive:

   __ONCE_ALIGNMENT - for pthread_once_t/once_flag definition.

   And finally the internal pthread_rwlock_t (struct __pthread_rwlock_arch_t)
   must be defined.
 */
/* Copyright (C) 2002-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


#define _BITS_PTHREADTYPES_ARCH_H 1

/* Determine the wordsize from the preprocessor defines.  */


#define __WORDSIZE 64







#define __WORDSIZE_TIME64_COMPAT32 1
/* Both x86-64 and x32 use the 64-bit system call interface.  */
#define __SYSCALL_WORDSIZE 64



#define __SIZEOF_PTHREAD_MUTEX_T 40
#define __SIZEOF_PTHREAD_ATTR_T 56
#define __SIZEOF_PTHREAD_MUTEX_T 40
#define __SIZEOF_PTHREAD_RWLOCK_T 56
#define __SIZEOF_PTHREAD_BARRIER_T 32














#define __SIZEOF_PTHREAD_MUTEXATTR_T 4
#define __SIZEOF_PTHREAD_COND_T 48
#define __SIZEOF_PTHREAD_CONDATTR_T 4
#define __SIZEOF_PTHREAD_RWLOCKATTR_T 8
#define __SIZEOF_PTHREAD_BARRIERATTR_T 4

/* Definitions for internal mutex struct.  */
#define __PTHREAD_COMPAT_PADDING_MID 
#define __PTHREAD_COMPAT_PADDING_END 
#define __PTHREAD_MUTEX_LOCK_ELISION 1

#define __PTHREAD_MUTEX_NUSERS_AFTER_KIND 0
#define __PTHREAD_MUTEX_USE_UNION 0





#define __LOCK_ALIGNMENT 
#define __ONCE_ALIGNMENT 

struct __pthread_rwlock_arch_t
{
  unsigned int __readers;
  unsigned int __writers;
  unsigned int __wrphase_futex;
  unsigned int __writers_futex;
  unsigned int __pad3;
  unsigned int __pad4;

  int __cur_writer;
  int __shared;
  signed char __rwelision;




  unsigned char __pad1[7];
#define __PTHREAD_RWLOCK_ELISION_EXTRA 0, { 0, 0, 0, 0, 0, 0, 0 }

  unsigned long int __pad2;
  /* FLAGS must stay at this position in the structure to maintain
     binary compatibility.  */
  unsigned int __flags;
#define __PTHREAD_RWLOCK_INT_FLAGS_SHARED 1
};







/* Common definition of pthread_mutex_t. */


typedef struct __pthread_internal_list
{
  struct __pthread_internal_list *__prev;
  struct __pthread_internal_list *__next;
} __pthread_list_t;







/* Lock elision support.  */


#define __PTHREAD_SPINS_DATA short __spins; short __elision


#define __PTHREAD_SPINS 0, 0






struct __pthread_mutex_s
{
  int __lock __LOCK_ALIGNMENT;
  unsigned int __count;
  int __owner;

  unsigned int __nusers;

  /* KIND must stay at this position in the structure to maintain
     binary compatibility with static initializers.

     Concurrency notes:
     The __kind of a mutex is initialized either by the static
     PTHREAD_MUTEX_INITIALIZER or by a call to pthread_mutex_init.

     After a mutex has been initialized, the __kind of a mutex is usually not
     changed.  BUT it can be set to -1 in pthread_mutex_destroy or elision can
     be enabled.  This is done concurrently in the pthread_mutex_*lock functions
     by using the macro FORCE_ELISION. This macro is only defined for
     architectures which supports lock elision.

     For elision, there are the flags PTHREAD_MUTEX_ELISION_NP and
     PTHREAD_MUTEX_NO_ELISION_NP which can be set in addition to the already set
     type of a mutex.
     Before a mutex is initialized, only PTHREAD_MUTEX_NO_ELISION_NP can be set
     with pthread_mutexattr_settype.
     After a mutex has been initialized, the functions pthread_mutex_*lock can
     enable elision - if the mutex-type and the machine supports it - by setting
     the flag PTHREAD_MUTEX_ELISION_NP. This is done concurrently. Afterwards
     the lock / unlock functions are using specific elision code-paths.  */
  int __kind;
  __PTHREAD_COMPAT_PADDING_MID




  __PTHREAD_SPINS_DATA;
  __pthread_list_t __list;
#define __PTHREAD_MUTEX_HAVE_PREV 1
  __PTHREAD_COMPAT_PADDING_END
};


/* Common definition of pthread_cond_t. */

struct __pthread_cond_s
{
  __extension__ union
  {
    __extension__ unsigned long long int __wseq;
    struct
    {
      unsigned int __low;
      unsigned int __high;
    } __wseq32;
  };
  __extension__ union
  {
    __extension__ unsigned long long int __g1_start;
    struct
    {
      unsigned int __low;
      unsigned int __high;
    } __g1_start32;
  };
  unsigned int __g_refs[2] __LOCK_ALIGNMENT;
  unsigned int __g_size[2];
  unsigned int __g1_orig_size;
  unsigned int __wrefs;
  unsigned int __g_signals[2];
};


/* Thread identifiers.  The structure of the attribute type is not
   exposed on purpose.  */
typedef unsigned long int pthread_t;


/* Data structures for mutex handling.  The structure of the attribute
   type is not exposed on purpose.  */
typedef union
{
  char __size[__SIZEOF_PTHREAD_MUTEXATTR_T];
  int __align;
} pthread_mutexattr_t;


/* Data structure for condition variable handling.  The structure of
   the attribute type is not exposed on purpose.  */
typedef union
{
  char __size[__SIZEOF_PTHREAD_CONDATTR_T];
  int __align;
} pthread_condattr_t;


/* Keys for thread-specific data */
typedef unsigned int pthread_key_t;


/* Once-only execution */
typedef int __ONCE_ALIGNMENT pthread_once_t;


union pthread_attr_t
{
  char __size[__SIZEOF_PTHREAD_ATTR_T];
  long int __align;
};

typedef union pthread_attr_t pthread_attr_t;
#define __have_pthread_attr_t 1



typedef union
{
  struct __pthread_mutex_s __data;
  char __size[__SIZEOF_PTHREAD_MUTEX_T];
  long int __align;
} pthread_mutex_t;


typedef union
{
  struct __pthread_cond_s __data;
  char __size[__SIZEOF_PTHREAD_COND_T];
  __extension__ long long int __align;
} pthread_cond_t;



/* Data structure for reader-writer lock variable handling.  The
   structure of the attribute type is deliberately not exposed.  */
typedef union
{
  struct __pthread_rwlock_arch_t __data;
  char __size[__SIZEOF_PTHREAD_RWLOCK_T];
  long int __align;
} pthread_rwlock_t;

typedef union
{
  char __size[__SIZEOF_PTHREAD_RWLOCKATTR_T];
  long int __align;
} pthread_rwlockattr_t;




/* POSIX spinlock data type.  */
typedef volatile int pthread_spinlock_t;


/* POSIX barriers data type.  The structure of the type is
   deliberately not exposed.  */
typedef union
{
  char __size[__SIZEOF_PTHREAD_BARRIER_T];
  long int __align;
} pthread_barrier_t;

typedef union
{
  char __size[__SIZEOF_PTHREAD_BARRIERATTR_T];
  int __align;
} pthread_barrierattr_t;


/* Copyright (C) 2001-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Define the machine-dependent type `jmp_buf'.  x86-64 version.  */

#define _BITS_SETJMP_H 1





/* Determine the wordsize from the preprocessor defines.  */


#define __WORDSIZE 64







#define __WORDSIZE_TIME64_COMPAT32 1
/* Both x86-64 and x32 use the 64-bit system call interface.  */
#define __SYSCALL_WORDSIZE 64




typedef long int __jmp_buf[8];








/* Determine the wordsize from the preprocessor defines.  */


#define __WORDSIZE 64







#define __WORDSIZE_TIME64_COMPAT32 1
/* Both x86-64 and x32 use the 64-bit system call interface.  */
#define __SYSCALL_WORDSIZE 64



/* Detach state.  */
enum
{
  PTHREAD_CREATE_JOINABLE,
#define PTHREAD_CREATE_JOINABLE PTHREAD_CREATE_JOINABLE
  PTHREAD_CREATE_DETACHED
#define PTHREAD_CREATE_DETACHED PTHREAD_CREATE_DETACHED
};


/* Mutex types.  */
enum
{
  PTHREAD_MUTEX_TIMED_NP,
  PTHREAD_MUTEX_RECURSIVE_NP,
  PTHREAD_MUTEX_ERRORCHECK_NP,
  PTHREAD_MUTEX_ADAPTIVE_NP

  ,
  PTHREAD_MUTEX_NORMAL = PTHREAD_MUTEX_TIMED_NP,
  PTHREAD_MUTEX_RECURSIVE = PTHREAD_MUTEX_RECURSIVE_NP,
  PTHREAD_MUTEX_ERRORCHECK = PTHREAD_MUTEX_ERRORCHECK_NP,
  PTHREAD_MUTEX_DEFAULT = PTHREAD_MUTEX_NORMAL


  /* For compatibility.  */
  , PTHREAD_MUTEX_FAST_NP = PTHREAD_MUTEX_TIMED_NP

};



/* Robust mutex or not flags.  */
enum
{
  PTHREAD_MUTEX_STALLED,
  PTHREAD_MUTEX_STALLED_NP = PTHREAD_MUTEX_STALLED,
  PTHREAD_MUTEX_ROBUST,
  PTHREAD_MUTEX_ROBUST_NP = PTHREAD_MUTEX_ROBUST
};




/* Mutex protocols.  */
enum
{
  PTHREAD_PRIO_NONE,
  PTHREAD_PRIO_INHERIT,
  PTHREAD_PRIO_PROTECT
};




#define PTHREAD_MUTEX_INITIALIZER { { 0, 0, 0, 0, 0, __PTHREAD_SPINS, { 0, 0 } } }


#define PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP { { 0, 0, 0, 0, PTHREAD_MUTEX_RECURSIVE_NP, __PTHREAD_SPINS, { 0, 0 } } }

#define PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP { { 0, 0, 0, 0, PTHREAD_MUTEX_ERRORCHECK_NP, __PTHREAD_SPINS, { 0, 0 } } }

#define PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP { { 0, 0, 0, 0, PTHREAD_MUTEX_ADAPTIVE_NP, __PTHREAD_SPINS, { 0, 0 } } }





/* Read-write lock types.  */

enum
{
  PTHREAD_RWLOCK_PREFER_READER_NP,
  PTHREAD_RWLOCK_PREFER_WRITER_NP,
  PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP,
  PTHREAD_RWLOCK_DEFAULT_NP = PTHREAD_RWLOCK_PREFER_READER_NP
};

/* Define __PTHREAD_RWLOCK_INT_FLAGS_SHARED to 1 if pthread_rwlock_t
   has the shared field.  All 64-bit architectures have the shared field
   in pthread_rwlock_t.  */






/* Read-write lock initializers.  */
#define PTHREAD_RWLOCK_INITIALIZER { { 0, 0, 0, 0, 0, 0, 0, 0, __PTHREAD_RWLOCK_ELISION_EXTRA, 0, 0 } }



#define PTHREAD_RWLOCK_WRITER_NONRECURSIVE_INITIALIZER_NP { { 0, 0, 0, 0, 0, 0, 0, 0, __PTHREAD_RWLOCK_ELISION_EXTRA, 0, PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP } }






/* Scheduler inheritance.  */
enum
{
  PTHREAD_INHERIT_SCHED,
#define PTHREAD_INHERIT_SCHED PTHREAD_INHERIT_SCHED
  PTHREAD_EXPLICIT_SCHED
#define PTHREAD_EXPLICIT_SCHED PTHREAD_EXPLICIT_SCHED
};


/* Scope handling.  */
enum
{
  PTHREAD_SCOPE_SYSTEM,
#define PTHREAD_SCOPE_SYSTEM PTHREAD_SCOPE_SYSTEM
  PTHREAD_SCOPE_PROCESS
#define PTHREAD_SCOPE_PROCESS PTHREAD_SCOPE_PROCESS
};


/* Process shared or private flag.  */
enum
{
  PTHREAD_PROCESS_PRIVATE,
#define PTHREAD_PROCESS_PRIVATE PTHREAD_PROCESS_PRIVATE
  PTHREAD_PROCESS_SHARED
#define PTHREAD_PROCESS_SHARED PTHREAD_PROCESS_SHARED
};



/* Conditional variable handling.  */
#define PTHREAD_COND_INITIALIZER { { {0}, {0}, {0, 0}, {0, 0}, 0, 0, {0, 0} } }


/* Cleanup buffers */
struct _pthread_cleanup_buffer
{
  void (*__routine) (void *);             /* Function to call.  */
  void *__arg;                            /* Its argument.  */
  int __canceltype;                       /* Saved cancellation type. */
  struct _pthread_cleanup_buffer *__prev; /* Chaining of cleanup functions.  */
};



#pragma GCC visibility pop

import  "./pr99023_a.H";



namespace std _GLIBCXX_VISIBILITY2(default)
{
} // namespace

