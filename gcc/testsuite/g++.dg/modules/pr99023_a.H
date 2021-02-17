# 0 "./pr99023_a.H"
// PR c++/99023, ICE
// { dg-additional-options {-std=c++17 -Wno-pedantic -fmodule-header -fpreprocessed -fdirectives-only --param ggc-min-expand=0} }
// { dg-module-cmi {,/pr99023_a.H} }
# 0 "<built-in>"
#define __STDC__ 1
# 0 "<built-in>"
#define __cplusplus 201703L
# 0 "<built-in>"
#define __STDC_UTF_16__ 1
# 0 "<built-in>"
#define __STDC_UTF_32__ 1
# 0 "<built-in>"
#define __STDC_HOSTED__ 1
# 0 "<built-in>"
#define __GNUC__ 11
# 0 "<built-in>"
#define __GNUC_MINOR__ 0
# 0 "<built-in>"
#define __GNUC_PATCHLEVEL__ 0
# 0 "<built-in>"
#define __VERSION__ "11.0.0 20210217 (experimental)"
# 0 "<built-in>"
#define __ATOMIC_RELAXED 0
# 0 "<built-in>"
#define __ATOMIC_SEQ_CST 5
# 0 "<built-in>"
#define __ATOMIC_ACQUIRE 2
# 0 "<built-in>"
#define __ATOMIC_RELEASE 3
# 0 "<built-in>"
#define __ATOMIC_ACQ_REL 4
# 0 "<built-in>"
#define __ATOMIC_CONSUME 1
# 0 "<built-in>"
#define __FINITE_MATH_ONLY__ 0
# 0 "<built-in>"
#define _LP64 1
# 0 "<built-in>"
#define __LP64__ 1
# 0 "<built-in>"
#define __SIZEOF_INT__ 4
# 0 "<built-in>"
#define __SIZEOF_LONG__ 8
# 0 "<built-in>"
#define __SIZEOF_LONG_LONG__ 8
# 0 "<built-in>"
#define __SIZEOF_SHORT__ 2
# 0 "<built-in>"
#define __SIZEOF_FLOAT__ 4
# 0 "<built-in>"
#define __SIZEOF_DOUBLE__ 8
# 0 "<built-in>"
#define __SIZEOF_LONG_DOUBLE__ 16
# 0 "<built-in>"
#define __SIZEOF_SIZE_T__ 8
# 0 "<built-in>"
#define __CHAR_BIT__ 8
# 0 "<built-in>"
#define __BIGGEST_ALIGNMENT__ 16
# 0 "<built-in>"
#define __ORDER_LITTLE_ENDIAN__ 1234
# 0 "<built-in>"
#define __ORDER_BIG_ENDIAN__ 4321
# 0 "<built-in>"
#define __ORDER_PDP_ENDIAN__ 3412
# 0 "<built-in>"
#define __BYTE_ORDER__ __ORDER_LITTLE_ENDIAN__
# 0 "<built-in>"
#define __FLOAT_WORD_ORDER__ __ORDER_LITTLE_ENDIAN__
# 0 "<built-in>"
#define __SIZEOF_POINTER__ 8
# 0 "<built-in>"
#define __GNUC_EXECUTION_CHARSET_NAME "UTF-8"
# 0 "<built-in>"
#define __GNUC_WIDE_EXECUTION_CHARSET_NAME "UTF-32LE"
# 0 "<built-in>"
#define __GNUG__ 11
# 0 "<built-in>"
#define __SIZE_TYPE__ long unsigned int
# 0 "<built-in>"
#define __PTRDIFF_TYPE__ long int
# 0 "<built-in>"
#define __WCHAR_TYPE__ int
# 0 "<built-in>"
#define __WINT_TYPE__ unsigned int
# 0 "<built-in>"
#define __INTMAX_TYPE__ long int
# 0 "<built-in>"
#define __UINTMAX_TYPE__ long unsigned int
# 0 "<built-in>"
#define __CHAR16_TYPE__ short unsigned int
# 0 "<built-in>"
#define __CHAR32_TYPE__ unsigned int
# 0 "<built-in>"
#define __SIG_ATOMIC_TYPE__ int
# 0 "<built-in>"
#define __INT8_TYPE__ signed char
# 0 "<built-in>"
#define __INT16_TYPE__ short int
# 0 "<built-in>"
#define __INT32_TYPE__ int
# 0 "<built-in>"
#define __INT64_TYPE__ long int
# 0 "<built-in>"
#define __UINT8_TYPE__ unsigned char
# 0 "<built-in>"
#define __UINT16_TYPE__ short unsigned int
# 0 "<built-in>"
#define __UINT32_TYPE__ unsigned int
# 0 "<built-in>"
#define __UINT64_TYPE__ long unsigned int
# 0 "<built-in>"
#define __INT_LEAST8_TYPE__ signed char
# 0 "<built-in>"
#define __INT_LEAST16_TYPE__ short int
# 0 "<built-in>"
#define __INT_LEAST32_TYPE__ int
# 0 "<built-in>"
#define __INT_LEAST64_TYPE__ long int
# 0 "<built-in>"
#define __UINT_LEAST8_TYPE__ unsigned char
# 0 "<built-in>"
#define __UINT_LEAST16_TYPE__ short unsigned int
# 0 "<built-in>"
#define __UINT_LEAST32_TYPE__ unsigned int
# 0 "<built-in>"
#define __UINT_LEAST64_TYPE__ long unsigned int
# 0 "<built-in>"
#define __INT_FAST8_TYPE__ signed char
# 0 "<built-in>"
#define __INT_FAST16_TYPE__ long int
# 0 "<built-in>"
#define __INT_FAST32_TYPE__ long int
# 0 "<built-in>"
#define __INT_FAST64_TYPE__ long int
# 0 "<built-in>"
#define __UINT_FAST8_TYPE__ unsigned char
# 0 "<built-in>"
#define __UINT_FAST16_TYPE__ long unsigned int
# 0 "<built-in>"
#define __UINT_FAST32_TYPE__ long unsigned int
# 0 "<built-in>"
#define __UINT_FAST64_TYPE__ long unsigned int
# 0 "<built-in>"
#define __INTPTR_TYPE__ long int
# 0 "<built-in>"
#define __UINTPTR_TYPE__ long unsigned int
# 0 "<built-in>"
#define __GXX_WEAK__ 1
# 0 "<built-in>"
#define __DEPRECATED 1
# 0 "<built-in>"
#define __GXX_RTTI 1
# 0 "<built-in>"
#define __cpp_rtti 199711L
# 0 "<built-in>"
#define __GXX_EXPERIMENTAL_CXX0X__ 1
# 0 "<built-in>"
#define __cpp_binary_literals 201304L
# 0 "<built-in>"
#define __cpp_hex_float 201603L
# 0 "<built-in>"
#define __cpp_runtime_arrays 198712L
# 0 "<built-in>"
#define __cpp_raw_strings 200710L
# 0 "<built-in>"
#define __cpp_unicode_literals 200710L
# 0 "<built-in>"
#define __cpp_user_defined_literals 200809L
# 0 "<built-in>"
#define __cpp_lambdas 200907L
# 0 "<built-in>"
#define __cpp_decltype 200707L
# 0 "<built-in>"
#define __cpp_attributes 200809L
# 0 "<built-in>"
#define __cpp_rvalue_reference 200610L
# 0 "<built-in>"
#define __cpp_rvalue_references 200610L
# 0 "<built-in>"
#define __cpp_variadic_templates 200704L
# 0 "<built-in>"
#define __cpp_initializer_lists 200806L
# 0 "<built-in>"
#define __cpp_delegating_constructors 200604L
# 0 "<built-in>"
#define __cpp_nsdmi 200809L
# 0 "<built-in>"
#define __cpp_inheriting_constructors 201511L
# 0 "<built-in>"
#define __cpp_ref_qualifiers 200710L
# 0 "<built-in>"
#define __cpp_alias_templates 200704L
# 0 "<built-in>"
#define __cpp_return_type_deduction 201304L
# 0 "<built-in>"
#define __cpp_init_captures 201304L
# 0 "<built-in>"
#define __cpp_generic_lambdas 201304L
# 0 "<built-in>"
#define __cpp_decltype_auto 201304L
# 0 "<built-in>"
#define __cpp_aggregate_nsdmi 201304L
# 0 "<built-in>"
#define __cpp_variable_templates 201304L
# 0 "<built-in>"
#define __cpp_digit_separators 201309L
# 0 "<built-in>"
#define __cpp_unicode_characters 201411L
# 0 "<built-in>"
#define __cpp_static_assert 201411L
# 0 "<built-in>"
#define __cpp_namespace_attributes 201411L
# 0 "<built-in>"
#define __cpp_enumerator_attributes 201411L
# 0 "<built-in>"
#define __cpp_nested_namespace_definitions 201411L
# 0 "<built-in>"
#define __cpp_fold_expressions 201603L
# 0 "<built-in>"
#define __cpp_nontype_template_args 201411L
# 0 "<built-in>"
#define __cpp_range_based_for 201603L
# 0 "<built-in>"
#define __cpp_constexpr 201603L
# 0 "<built-in>"
#define __cpp_if_constexpr 201606L
# 0 "<built-in>"
#define __cpp_capture_star_this 201603L
# 0 "<built-in>"
#define __cpp_inline_variables 201606L
# 0 "<built-in>"
#define __cpp_aggregate_bases 201603L
# 0 "<built-in>"
#define __cpp_deduction_guides 201703L
# 0 "<built-in>"
#define __cpp_noexcept_function_type 201510L
# 0 "<built-in>"
#define __cpp_template_auto 201606L
# 0 "<built-in>"
#define __cpp_structured_bindings 201606L
# 0 "<built-in>"
#define __cpp_variadic_using 201611L
# 0 "<built-in>"
#define __cpp_guaranteed_copy_elision 201606L
# 0 "<built-in>"
#define __cpp_nontype_template_parameter_auto 201606L
# 0 "<built-in>"
#define __cpp_modules 201810L
# 0 "<built-in>"
#define __cpp_sized_deallocation 201309L
# 0 "<built-in>"
#define __cpp_aligned_new 201606L
# 0 "<built-in>"
#define __STDCPP_DEFAULT_NEW_ALIGNMENT__ 16
# 0 "<built-in>"
#define __cpp_template_template_args 201611L
# 0 "<built-in>"
#define __cpp_threadsafe_static_init 200806L
# 0 "<built-in>"
#define __STDCPP_THREADS__ 1
# 0 "<built-in>"
#define __EXCEPTIONS 1
# 0 "<built-in>"
#define __cpp_exceptions 199711L
# 0 "<built-in>"
#define __GXX_ABI_VERSION 1015
# 0 "<built-in>"
#define __SCHAR_MAX__ 0x7f
# 0 "<built-in>"
#define __SHRT_MAX__ 0x7fff
# 0 "<built-in>"
#define __INT_MAX__ 0x7fffffff
# 0 "<built-in>"
#define __LONG_MAX__ 0x7fffffffffffffffL
# 0 "<built-in>"
#define __LONG_LONG_MAX__ 0x7fffffffffffffffLL
# 0 "<built-in>"
#define __WCHAR_MAX__ 0x7fffffff
# 0 "<built-in>"
#define __WCHAR_MIN__ (-__WCHAR_MAX__ - 1)
# 0 "<built-in>"
#define __WINT_MAX__ 0xffffffffU
# 0 "<built-in>"
#define __WINT_MIN__ 0U
# 0 "<built-in>"
#define __PTRDIFF_MAX__ 0x7fffffffffffffffL
# 0 "<built-in>"
#define __SIZE_MAX__ 0xffffffffffffffffUL
# 0 "<built-in>"
#define __SCHAR_WIDTH__ 8
# 0 "<built-in>"
#define __SHRT_WIDTH__ 16
# 0 "<built-in>"
#define __INT_WIDTH__ 32
# 0 "<built-in>"
#define __LONG_WIDTH__ 64
# 0 "<built-in>"
#define __LONG_LONG_WIDTH__ 64
# 0 "<built-in>"
#define __WCHAR_WIDTH__ 32
# 0 "<built-in>"
#define __WINT_WIDTH__ 32
# 0 "<built-in>"
#define __PTRDIFF_WIDTH__ 64
# 0 "<built-in>"
#define __SIZE_WIDTH__ 64
# 0 "<built-in>"
#define __INTMAX_MAX__ 0x7fffffffffffffffL
# 0 "<built-in>"
#define __INTMAX_C(c) c ## L
# 0 "<built-in>"
#define __UINTMAX_MAX__ 0xffffffffffffffffUL
# 0 "<built-in>"
#define __UINTMAX_C(c) c ## UL
# 0 "<built-in>"
#define __INTMAX_WIDTH__ 64
# 0 "<built-in>"
#define __SIG_ATOMIC_MAX__ 0x7fffffff
# 0 "<built-in>"
#define __SIG_ATOMIC_MIN__ (-__SIG_ATOMIC_MAX__ - 1)
# 0 "<built-in>"
#define __SIG_ATOMIC_WIDTH__ 32
# 0 "<built-in>"
#define __INT8_MAX__ 0x7f
# 0 "<built-in>"
#define __INT16_MAX__ 0x7fff
# 0 "<built-in>"
#define __INT32_MAX__ 0x7fffffff
# 0 "<built-in>"
#define __INT64_MAX__ 0x7fffffffffffffffL
# 0 "<built-in>"
#define __UINT8_MAX__ 0xff
# 0 "<built-in>"
#define __UINT16_MAX__ 0xffff
# 0 "<built-in>"
#define __UINT32_MAX__ 0xffffffffU
# 0 "<built-in>"
#define __UINT64_MAX__ 0xffffffffffffffffUL
# 0 "<built-in>"
#define __INT_LEAST8_MAX__ 0x7f
# 0 "<built-in>"
#define __INT8_C(c) c
# 0 "<built-in>"
#define __INT_LEAST8_WIDTH__ 8
# 0 "<built-in>"
#define __INT_LEAST16_MAX__ 0x7fff
# 0 "<built-in>"
#define __INT16_C(c) c
# 0 "<built-in>"
#define __INT_LEAST16_WIDTH__ 16
# 0 "<built-in>"
#define __INT_LEAST32_MAX__ 0x7fffffff
# 0 "<built-in>"
#define __INT32_C(c) c
# 0 "<built-in>"
#define __INT_LEAST32_WIDTH__ 32
# 0 "<built-in>"
#define __INT_LEAST64_MAX__ 0x7fffffffffffffffL
# 0 "<built-in>"
#define __INT64_C(c) c ## L
# 0 "<built-in>"
#define __INT_LEAST64_WIDTH__ 64
# 0 "<built-in>"
#define __UINT_LEAST8_MAX__ 0xff
# 0 "<built-in>"
#define __UINT8_C(c) c
# 0 "<built-in>"
#define __UINT_LEAST16_MAX__ 0xffff
# 0 "<built-in>"
#define __UINT16_C(c) c
# 0 "<built-in>"
#define __UINT_LEAST32_MAX__ 0xffffffffU
# 0 "<built-in>"
#define __UINT32_C(c) c ## U
# 0 "<built-in>"
#define __UINT_LEAST64_MAX__ 0xffffffffffffffffUL
# 0 "<built-in>"
#define __UINT64_C(c) c ## UL
# 0 "<built-in>"
#define __INT_FAST8_MAX__ 0x7f
# 0 "<built-in>"
#define __INT_FAST8_WIDTH__ 8
# 0 "<built-in>"
#define __INT_FAST16_MAX__ 0x7fffffffffffffffL
# 0 "<built-in>"
#define __INT_FAST16_WIDTH__ 64
# 0 "<built-in>"
#define __INT_FAST32_MAX__ 0x7fffffffffffffffL
# 0 "<built-in>"
#define __INT_FAST32_WIDTH__ 64
# 0 "<built-in>"
#define __INT_FAST64_MAX__ 0x7fffffffffffffffL
# 0 "<built-in>"
#define __INT_FAST64_WIDTH__ 64
# 0 "<built-in>"
#define __UINT_FAST8_MAX__ 0xff
# 0 "<built-in>"
#define __UINT_FAST16_MAX__ 0xffffffffffffffffUL
# 0 "<built-in>"
#define __UINT_FAST32_MAX__ 0xffffffffffffffffUL
# 0 "<built-in>"
#define __UINT_FAST64_MAX__ 0xffffffffffffffffUL
# 0 "<built-in>"
#define __INTPTR_MAX__ 0x7fffffffffffffffL
# 0 "<built-in>"
#define __INTPTR_WIDTH__ 64
# 0 "<built-in>"
#define __UINTPTR_MAX__ 0xffffffffffffffffUL
# 0 "<built-in>"
#define __GCC_IEC_559 2
# 0 "<built-in>"
#define __GCC_IEC_559_COMPLEX 2
# 0 "<built-in>"
#define __FLT_EVAL_METHOD__ 0
# 0 "<built-in>"
#define __FLT_EVAL_METHOD_TS_18661_3__ 0
# 0 "<built-in>"
#define __DEC_EVAL_METHOD__ 2
# 0 "<built-in>"
#define __FLT_RADIX__ 2
# 0 "<built-in>"
#define __FLT_MANT_DIG__ 24
# 0 "<built-in>"
#define __FLT_DIG__ 6
# 0 "<built-in>"
#define __FLT_MIN_EXP__ (-125)
# 0 "<built-in>"
#define __FLT_MIN_10_EXP__ (-37)
# 0 "<built-in>"
#define __FLT_MAX_EXP__ 128
# 0 "<built-in>"
#define __FLT_MAX_10_EXP__ 38
# 0 "<built-in>"
#define __FLT_DECIMAL_DIG__ 9
# 0 "<built-in>"
#define __FLT_MAX__ 3.40282346638528859811704183484516925e+38F
# 0 "<built-in>"
#define __FLT_NORM_MAX__ 3.40282346638528859811704183484516925e+38F
# 0 "<built-in>"
#define __FLT_MIN__ 1.17549435082228750796873653722224568e-38F
# 0 "<built-in>"
#define __FLT_EPSILON__ 1.19209289550781250000000000000000000e-7F
# 0 "<built-in>"
#define __FLT_DENORM_MIN__ 1.40129846432481707092372958328991613e-45F
# 0 "<built-in>"
#define __FLT_HAS_DENORM__ 1
# 0 "<built-in>"
#define __FLT_HAS_INFINITY__ 1
# 0 "<built-in>"
#define __FLT_HAS_QUIET_NAN__ 1
# 0 "<built-in>"
#define __FLT_IS_IEC_60559__ 2
# 0 "<built-in>"
#define __DBL_MANT_DIG__ 53
# 0 "<built-in>"
#define __DBL_DIG__ 15
# 0 "<built-in>"
#define __DBL_MIN_EXP__ (-1021)
# 0 "<built-in>"
#define __DBL_MIN_10_EXP__ (-307)
# 0 "<built-in>"
#define __DBL_MAX_EXP__ 1024
# 0 "<built-in>"
#define __DBL_MAX_10_EXP__ 308
# 0 "<built-in>"
#define __DBL_DECIMAL_DIG__ 17
# 0 "<built-in>"
#define __DBL_MAX__ double(1.79769313486231570814527423731704357e+308L)
# 0 "<built-in>"
#define __DBL_NORM_MAX__ double(1.79769313486231570814527423731704357e+308L)
# 0 "<built-in>"
#define __DBL_MIN__ double(2.22507385850720138309023271733240406e-308L)
# 0 "<built-in>"
#define __DBL_EPSILON__ double(2.22044604925031308084726333618164062e-16L)
# 0 "<built-in>"
#define __DBL_DENORM_MIN__ double(4.94065645841246544176568792868221372e-324L)
# 0 "<built-in>"
#define __DBL_HAS_DENORM__ 1
# 0 "<built-in>"
#define __DBL_HAS_INFINITY__ 1
# 0 "<built-in>"
#define __DBL_HAS_QUIET_NAN__ 1
# 0 "<built-in>"
#define __DBL_IS_IEC_60559__ 2
# 0 "<built-in>"
#define __LDBL_MANT_DIG__ 64
# 0 "<built-in>"
#define __LDBL_DIG__ 18
# 0 "<built-in>"
#define __LDBL_MIN_EXP__ (-16381)
# 0 "<built-in>"
#define __LDBL_MIN_10_EXP__ (-4931)
# 0 "<built-in>"
#define __LDBL_MAX_EXP__ 16384
# 0 "<built-in>"
#define __LDBL_MAX_10_EXP__ 4932
# 0 "<built-in>"
#define __DECIMAL_DIG__ 21
# 0 "<built-in>"
#define __LDBL_DECIMAL_DIG__ 21
# 0 "<built-in>"
#define __LDBL_MAX__ 1.18973149535723176502126385303097021e+4932L
# 0 "<built-in>"
#define __LDBL_NORM_MAX__ 1.18973149535723176502126385303097021e+4932L
# 0 "<built-in>"
#define __LDBL_MIN__ 3.36210314311209350626267781732175260e-4932L
# 0 "<built-in>"
#define __LDBL_EPSILON__ 1.08420217248550443400745280086994171e-19L
# 0 "<built-in>"
#define __LDBL_DENORM_MIN__ 3.64519953188247460252840593361941982e-4951L
# 0 "<built-in>"
#define __LDBL_HAS_DENORM__ 1
# 0 "<built-in>"
#define __LDBL_HAS_INFINITY__ 1
# 0 "<built-in>"
#define __LDBL_HAS_QUIET_NAN__ 1
# 0 "<built-in>"
#define __LDBL_IS_IEC_60559__ 2
# 0 "<built-in>"
#define __FLT32_MANT_DIG__ 24
# 0 "<built-in>"
#define __FLT32_DIG__ 6
# 0 "<built-in>"
#define __FLT32_MIN_EXP__ (-125)
# 0 "<built-in>"
#define __FLT32_MIN_10_EXP__ (-37)
# 0 "<built-in>"
#define __FLT32_MAX_EXP__ 128
# 0 "<built-in>"
#define __FLT32_MAX_10_EXP__ 38
# 0 "<built-in>"
#define __FLT32_DECIMAL_DIG__ 9
# 0 "<built-in>"
#define __FLT32_MAX__ 3.40282346638528859811704183484516925e+38F32
# 0 "<built-in>"
#define __FLT32_NORM_MAX__ 3.40282346638528859811704183484516925e+38F32
# 0 "<built-in>"
#define __FLT32_MIN__ 1.17549435082228750796873653722224568e-38F32
# 0 "<built-in>"
#define __FLT32_EPSILON__ 1.19209289550781250000000000000000000e-7F32
# 0 "<built-in>"
#define __FLT32_DENORM_MIN__ 1.40129846432481707092372958328991613e-45F32
# 0 "<built-in>"
#define __FLT32_HAS_DENORM__ 1
# 0 "<built-in>"
#define __FLT32_HAS_INFINITY__ 1
# 0 "<built-in>"
#define __FLT32_HAS_QUIET_NAN__ 1
# 0 "<built-in>"
#define __FLT32_IS_IEC_60559__ 2
# 0 "<built-in>"
#define __FLT64_MANT_DIG__ 53
# 0 "<built-in>"
#define __FLT64_DIG__ 15
# 0 "<built-in>"
#define __FLT64_MIN_EXP__ (-1021)
# 0 "<built-in>"
#define __FLT64_MIN_10_EXP__ (-307)
# 0 "<built-in>"
#define __FLT64_MAX_EXP__ 1024
# 0 "<built-in>"
#define __FLT64_MAX_10_EXP__ 308
# 0 "<built-in>"
#define __FLT64_DECIMAL_DIG__ 17
# 0 "<built-in>"
#define __FLT64_MAX__ 1.79769313486231570814527423731704357e+308F64
# 0 "<built-in>"
#define __FLT64_NORM_MAX__ 1.79769313486231570814527423731704357e+308F64
# 0 "<built-in>"
#define __FLT64_MIN__ 2.22507385850720138309023271733240406e-308F64
# 0 "<built-in>"
#define __FLT64_EPSILON__ 2.22044604925031308084726333618164062e-16F64
# 0 "<built-in>"
#define __FLT64_DENORM_MIN__ 4.94065645841246544176568792868221372e-324F64
# 0 "<built-in>"
#define __FLT64_HAS_DENORM__ 1
# 0 "<built-in>"
#define __FLT64_HAS_INFINITY__ 1
# 0 "<built-in>"
#define __FLT64_HAS_QUIET_NAN__ 1
# 0 "<built-in>"
#define __FLT64_IS_IEC_60559__ 2
# 0 "<built-in>"
#define __FLT128_MANT_DIG__ 113
# 0 "<built-in>"
#define __FLT128_DIG__ 33
# 0 "<built-in>"
#define __FLT128_MIN_EXP__ (-16381)
# 0 "<built-in>"
#define __FLT128_MIN_10_EXP__ (-4931)
# 0 "<built-in>"
#define __FLT128_MAX_EXP__ 16384
# 0 "<built-in>"
#define __FLT128_MAX_10_EXP__ 4932
# 0 "<built-in>"
#define __FLT128_DECIMAL_DIG__ 36
# 0 "<built-in>"
#define __FLT128_MAX__ 1.18973149535723176508575932662800702e+4932F128
# 0 "<built-in>"
#define __FLT128_NORM_MAX__ 1.18973149535723176508575932662800702e+4932F128
# 0 "<built-in>"
#define __FLT128_MIN__ 3.36210314311209350626267781732175260e-4932F128
# 0 "<built-in>"
#define __FLT128_EPSILON__ 1.92592994438723585305597794258492732e-34F128
# 0 "<built-in>"
#define __FLT128_DENORM_MIN__ 6.47517511943802511092443895822764655e-4966F128
# 0 "<built-in>"
#define __FLT128_HAS_DENORM__ 1
# 0 "<built-in>"
#define __FLT128_HAS_INFINITY__ 1
# 0 "<built-in>"
#define __FLT128_HAS_QUIET_NAN__ 1
# 0 "<built-in>"
#define __FLT128_IS_IEC_60559__ 2
# 0 "<built-in>"
#define __FLT32X_MANT_DIG__ 53
# 0 "<built-in>"
#define __FLT32X_DIG__ 15
# 0 "<built-in>"
#define __FLT32X_MIN_EXP__ (-1021)
# 0 "<built-in>"
#define __FLT32X_MIN_10_EXP__ (-307)
# 0 "<built-in>"
#define __FLT32X_MAX_EXP__ 1024
# 0 "<built-in>"
#define __FLT32X_MAX_10_EXP__ 308
# 0 "<built-in>"
#define __FLT32X_DECIMAL_DIG__ 17
# 0 "<built-in>"
#define __FLT32X_MAX__ 1.79769313486231570814527423731704357e+308F32x
# 0 "<built-in>"
#define __FLT32X_NORM_MAX__ 1.79769313486231570814527423731704357e+308F32x
# 0 "<built-in>"
#define __FLT32X_MIN__ 2.22507385850720138309023271733240406e-308F32x
# 0 "<built-in>"
#define __FLT32X_EPSILON__ 2.22044604925031308084726333618164062e-16F32x
# 0 "<built-in>"
#define __FLT32X_DENORM_MIN__ 4.94065645841246544176568792868221372e-324F32x
# 0 "<built-in>"
#define __FLT32X_HAS_DENORM__ 1
# 0 "<built-in>"
#define __FLT32X_HAS_INFINITY__ 1
# 0 "<built-in>"
#define __FLT32X_HAS_QUIET_NAN__ 1
# 0 "<built-in>"
#define __FLT32X_IS_IEC_60559__ 2
# 0 "<built-in>"
#define __FLT64X_MANT_DIG__ 64
# 0 "<built-in>"
#define __FLT64X_DIG__ 18
# 0 "<built-in>"
#define __FLT64X_MIN_EXP__ (-16381)
# 0 "<built-in>"
#define __FLT64X_MIN_10_EXP__ (-4931)
# 0 "<built-in>"
#define __FLT64X_MAX_EXP__ 16384
# 0 "<built-in>"
#define __FLT64X_MAX_10_EXP__ 4932
# 0 "<built-in>"
#define __FLT64X_DECIMAL_DIG__ 21
# 0 "<built-in>"
#define __FLT64X_MAX__ 1.18973149535723176502126385303097021e+4932F64x
# 0 "<built-in>"
#define __FLT64X_NORM_MAX__ 1.18973149535723176502126385303097021e+4932F64x
# 0 "<built-in>"
#define __FLT64X_MIN__ 3.36210314311209350626267781732175260e-4932F64x
# 0 "<built-in>"
#define __FLT64X_EPSILON__ 1.08420217248550443400745280086994171e-19F64x
# 0 "<built-in>"
#define __FLT64X_DENORM_MIN__ 3.64519953188247460252840593361941982e-4951F64x
# 0 "<built-in>"
#define __FLT64X_HAS_DENORM__ 1
# 0 "<built-in>"
#define __FLT64X_HAS_INFINITY__ 1
# 0 "<built-in>"
#define __FLT64X_HAS_QUIET_NAN__ 1
# 0 "<built-in>"
#define __FLT64X_IS_IEC_60559__ 2
# 0 "<built-in>"
#define __DEC32_MANT_DIG__ 7
# 0 "<built-in>"
#define __DEC32_MIN_EXP__ (-94)
# 0 "<built-in>"
#define __DEC32_MAX_EXP__ 97
# 0 "<built-in>"
#define __DEC32_MIN__ 1E-95DF
# 0 "<built-in>"
#define __DEC32_MAX__ 9.999999E96DF
# 0 "<built-in>"
#define __DEC32_EPSILON__ 1E-6DF
# 0 "<built-in>"
#define __DEC32_SUBNORMAL_MIN__ 0.000001E-95DF
# 0 "<built-in>"
#define __DEC64_MANT_DIG__ 16
# 0 "<built-in>"
#define __DEC64_MIN_EXP__ (-382)
# 0 "<built-in>"
#define __DEC64_MAX_EXP__ 385
# 0 "<built-in>"
#define __DEC64_MIN__ 1E-383DD
# 0 "<built-in>"
#define __DEC64_MAX__ 9.999999999999999E384DD
# 0 "<built-in>"
#define __DEC64_EPSILON__ 1E-15DD
# 0 "<built-in>"
#define __DEC64_SUBNORMAL_MIN__ 0.000000000000001E-383DD
# 0 "<built-in>"
#define __DEC128_MANT_DIG__ 34
# 0 "<built-in>"
#define __DEC128_MIN_EXP__ (-6142)
# 0 "<built-in>"
#define __DEC128_MAX_EXP__ 6145
# 0 "<built-in>"
#define __DEC128_MIN__ 1E-6143DL
# 0 "<built-in>"
#define __DEC128_MAX__ 9.999999999999999999999999999999999E6144DL
# 0 "<built-in>"
#define __DEC128_EPSILON__ 1E-33DL
# 0 "<built-in>"
#define __DEC128_SUBNORMAL_MIN__ 0.000000000000000000000000000000001E-6143DL
# 0 "<built-in>"
#define __REGISTER_PREFIX__ 
# 0 "<built-in>"
#define __USER_LABEL_PREFIX__ 
# 0 "<built-in>"
#define __GNUC_STDC_INLINE__ 1
# 0 "<built-in>"
#define __NO_INLINE__ 1
# 0 "<built-in>"
#define __STRICT_ANSI__ 1
# 0 "<built-in>"
#define __GCC_HAVE_SYNC_COMPARE_AND_SWAP_1 1
# 0 "<built-in>"
#define __GCC_HAVE_SYNC_COMPARE_AND_SWAP_2 1
# 0 "<built-in>"
#define __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4 1
# 0 "<built-in>"
#define __GCC_HAVE_SYNC_COMPARE_AND_SWAP_8 1
# 0 "<built-in>"
#define __GCC_ATOMIC_BOOL_LOCK_FREE 2
# 0 "<built-in>"
#define __GCC_ATOMIC_CHAR_LOCK_FREE 2
# 0 "<built-in>"
#define __GCC_ATOMIC_CHAR16_T_LOCK_FREE 2
# 0 "<built-in>"
#define __GCC_ATOMIC_CHAR32_T_LOCK_FREE 2
# 0 "<built-in>"
#define __GCC_ATOMIC_WCHAR_T_LOCK_FREE 2
# 0 "<built-in>"
#define __GCC_ATOMIC_SHORT_LOCK_FREE 2
# 0 "<built-in>"
#define __GCC_ATOMIC_INT_LOCK_FREE 2
# 0 "<built-in>"
#define __GCC_ATOMIC_LONG_LOCK_FREE 2
# 0 "<built-in>"
#define __GCC_ATOMIC_LLONG_LOCK_FREE 2
# 0 "<built-in>"
#define __GCC_ATOMIC_TEST_AND_SET_TRUEVAL 1
# 0 "<built-in>"
#define __GCC_ATOMIC_POINTER_LOCK_FREE 2
# 0 "<built-in>"
#define __HAVE_SPECULATION_SAFE_VALUE 1
# 0 "<built-in>"
#define __GCC_HAVE_DWARF2_CFI_ASM 1
# 0 "<built-in>"
#define __PRAGMA_REDEFINE_EXTNAME 1
# 0 "<built-in>"
#define __SIZEOF_INT128__ 16
# 0 "<built-in>"
#define __SIZEOF_WCHAR_T__ 4
# 0 "<built-in>"
#define __SIZEOF_WINT_T__ 4
# 0 "<built-in>"
#define __SIZEOF_PTRDIFF_T__ 8
# 0 "<built-in>"
#define __amd64 1
# 0 "<built-in>"
#define __amd64__ 1
# 0 "<built-in>"
#define __x86_64 1
# 0 "<built-in>"
#define __x86_64__ 1
# 0 "<built-in>"
#define __SIZEOF_FLOAT80__ 16
# 0 "<built-in>"
#define __SIZEOF_FLOAT128__ 16
# 0 "<built-in>"
#define __ATOMIC_HLE_ACQUIRE 65536
# 0 "<built-in>"
#define __ATOMIC_HLE_RELEASE 131072
# 0 "<built-in>"
#define __GCC_ASM_FLAG_OUTPUTS__ 1
# 0 "<built-in>"
#define __k8 1
# 0 "<built-in>"
#define __k8__ 1
# 0 "<built-in>"
#define __code_model_small__ 1
# 0 "<built-in>"
#define __MMX__ 1
# 0 "<built-in>"
#define __SSE__ 1
# 0 "<built-in>"
#define __SSE2__ 1
# 0 "<built-in>"
#define __FXSR__ 1
# 0 "<built-in>"
#define __SSE_MATH__ 1
# 0 "<built-in>"
#define __SSE2_MATH__ 1
# 0 "<built-in>"
#define __MMX_WITH_SSE__ 1
# 0 "<built-in>"
#define __SEG_FS 1
# 0 "<built-in>"
#define __SEG_GS 1
# 0 "<built-in>"
#define __gnu_linux__ 1
# 0 "<built-in>"
#define __linux 1
# 0 "<built-in>"
#define __linux__ 1
# 0 "<built-in>"
#define __unix 1
# 0 "<built-in>"
#define __unix__ 1
# 0 "<built-in>"
#define __ELF__ 1
# 0 "<built-in>"
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
# 1 "../../..//src/libstdc++-v3/libsupc++/initializer_list"
// std::initializer_list support -*- C++ -*-

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

/** @file initializer_list
 *  This is a Standard C++ Library header.
 */


#define _INITIALIZER_LIST 

       
# 34 "../../..//src/libstdc++-v3/libsupc++/initializer_list" 3





#pragma GCC visibility push(default)

# 1 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/c++config.h" 1 3
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
# 350 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/c++config.h" 3
#define _GLIBCXX_BEGIN_NAMESPACE_VERSION 
#define _GLIBCXX_END_NAMESPACE_VERSION 


// Inline namespaces for special modes: debug, parallel.
# 392 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/c++config.h" 3

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
# 449 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/c++config.h" 3

# 459 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/c++config.h" 3
#define _GLIBCXX_NAMESPACE_LDBL 
#define _GLIBCXX_BEGIN_NAMESPACE_LDBL 
#define _GLIBCXX_END_NAMESPACE_LDBL 



#define _GLIBCXX_NAMESPACE_LDBL_OR_CXX11 _GLIBCXX_NAMESPACE_CXX11
#define _GLIBCXX_BEGIN_NAMESPACE_LDBL_OR_CXX11 _GLIBCXX_BEGIN_NAMESPACE_CXX11
#define _GLIBCXX_END_NAMESPACE_LDBL_OR_CXX11 _GLIBCXX_END_NAMESPACE_CXX11








// Debug Mode implies checking assertions.




// Disable std::string explicit instantiation declarations in order to assert.





// Assert.
# 509 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/c++config.h" 3




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
# 1 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/os_defines.h" 1 3
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

# 1 "/usr/include/features.h" 1 3 4
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


# 283 "/usr/include/features.h" 3 4

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


# 390 "/usr/include/features.h" 3 4

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


# 1 "/usr/include/sys/cdefs.h" 1 3 4
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








# 87 "/usr/include/sys/cdefs.h" 3 4

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
# 160 "/usr/include/sys/cdefs.h" 3 4


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

# define __REDIRECT(name, proto, alias) name proto; \
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
# 399 "/usr/include/sys/cdefs.h" 3 4



#define __glibc_unlikely(cond) __builtin_expect ((cond), 0)
#define __glibc_likely(cond) __builtin_expect ((cond), 1)






#define __glibc_has_attribute(attr) __has_attribute (attr)




# 424 "/usr/include/sys/cdefs.h" 3 4


/* Describes a char array whose address can safely be passed as the first
   argument to strncpy and strncat, as the char array is not necessarily
   a NUL-terminated string.  */
#define __attribute_nonstring__ __attribute__ ((__nonstring__))












# 1 "/usr/include/bits/wordsize.h" 1 3 4
/* Determine the wordsize from the preprocessor defines.  */


#define __WORDSIZE 64







#define __WORDSIZE_TIME64_COMPAT32 1
/* Both x86-64 and x32 use the 64-bit system call interface.  */
#define __SYSCALL_WORDSIZE 64
# 443 "/usr/include/sys/cdefs.h" 2 3 4
# 1 "/usr/include/bits/long-double.h" 1 3 4
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
# 444 "/usr/include/sys/cdefs.h" 2 3 4

# 464 "/usr/include/sys/cdefs.h" 3 4

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


# 429 "/usr/include/features.h" 2 3 4


/* If we don't have __REDIRECT, prototypes will be missing if
   __USE_FILE_OFFSET64 but not __USE_LARGEFILE[64]. */







/* Decide whether we can define 'extern inline' functions in headers.  */







/* This is here only because every header file already includes this one.
   Get the definitions of all the appropriate `__stub_FUNCTION' symbols.
   <gnu/stubs.h> contains `#define __stub_FUNCTION' when FUNCTION is a stub
   that will always return failure (and set errno to ENOSYS).  */
# 1 "/usr/include/gnu/stubs.h" 1 3 4
/* This file is automatically generated.
   This file selects the right generated file of `__stub_FUNCTION' macros
   based on the architecture being compiled for.  */






# 1 "/usr/include/gnu/stubs-64.h" 1 3 4
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
# 11 "/usr/include/gnu/stubs.h" 2 3 4

# 453 "/usr/include/features.h" 2 3 4


# 40 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/os_defines.h" 2 3

// Provide a declaration for the possibly deprecated gets function, as
// glibc 2.15 and later does not declare gets for ISO C11 when
// __GNU_SOURCE is defined.

#undef _GLIBCXX_HAVE_GETS


// Glibc 2.23 removed the obsolete isinf and isnan declarations. Check the
// version dynamically in case it has changed since libstdc++ was configured.
#define _GLIBCXX_NO_OBSOLETE_ISINF_ISNAN_DYNAMIC __GLIBC_PREREQ(2,23)


// Since glibc 2.27 pthread_self() is usable without linking to libpthread.
#define _GLIBCXX_NATIVE_THREAD_ID pthread_self()
# 63 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/os_defines.h" 3

# 569 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/c++config.h" 2 3

// Pick up any CPU-specific definitions.
# 1 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/cpu_defines.h" 1 3
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

# 572 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/c++config.h" 2 3

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

# 671 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/c++config.h" 3

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

# 1 "../x86_64-pc-linux-gnu/libstdc++-v3/include/pstl/pstl_config.h" 1 3
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
# 76 "../x86_64-pc-linux-gnu/libstdc++-v3/include/pstl/pstl_config.h" 3




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

# 779 "../x86_64-pc-linux-gnu/libstdc++-v3/include/x86_64-pc-linux-gnu/bits/c++config.h" 2 3



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


































































































































































































































































































































# 42 "../../..//src/libstdc++-v3/libsupc++/initializer_list" 2 3

namespace std
{
  /// initializer_list
  template<class _E>
    class initializer_list
    {
    public:
      typedef _E 		value_type;
      typedef const _E& 	reference;
      typedef const _E& 	const_reference;
      typedef size_t 		size_type;
      typedef const _E* 	iterator;
      typedef const _E* 	const_iterator;

    private:
      iterator			_M_array;
      size_type			_M_len;

      // The compiler can call a private constructor.
      constexpr initializer_list(const_iterator __a, size_type __l)
      : _M_array(__a), _M_len(__l) { }

    public:
      constexpr initializer_list() noexcept
      : _M_array(0), _M_len(0) { }

      // Number of elements.
      constexpr size_type
      size() const noexcept { return _M_len; }

      // First element.
      constexpr const_iterator
      begin() const noexcept { return _M_array; }

      // One past the last element.
      constexpr const_iterator
      end() const noexcept { return begin() + size(); }
    };

  /**
   *  @brief  Return an iterator pointing to the first element of
   *          the initializer_list.
   *  @param  __ils  Initializer list.
   *  @relates initializer_list
   */
  template<class _Tp>
    constexpr const _Tp*
    begin(initializer_list<_Tp> __ils) noexcept
    { return __ils.begin(); }

  /**
   *  @brief  Return an iterator pointing to one past the last element
   *          of the initializer_list.
   *  @param  __ils  Initializer list.
   *  @relates initializer_list
   */
  template<class _Tp>
    constexpr const _Tp*
    end(initializer_list<_Tp> __ils) noexcept
    { return __ils.end(); }
}

#pragma GCC visibility pop



