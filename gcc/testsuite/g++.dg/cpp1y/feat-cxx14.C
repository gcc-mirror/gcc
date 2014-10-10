// { dg-do compile { target c++1y } }
// { dg-options "-I${srcdir}/g++.dg/cpp1y -I${srcdir}/g++.dg/cpp1y/testinc" }

// Begin C++11 tests.

#ifndef __cpp_unicode_characters
#  error "__cpp_unicode_characters"
#elif __cpp_unicode_characters != 200704
#  error "__cpp_unicode_characters != 200704"
#endif

#ifndef __cpp_raw_strings
#  error "__cpp_raw_strings"
#elif __cpp_raw_strings != 200710
#  error "__cpp_raw_strings != 200710"
#endif

#ifndef __cpp_unicode_literals
#  error "__cpp_unicode_literals"
#elif __cpp_unicode_literals != 200710
#  error "__cpp_unicode_literals != 200710"
#endif

#ifndef __cpp_user_defined_literals
#  error "__cpp_user_defined_literals"
#elif __cpp_user_defined_literals != 200809
#  error "__cpp_user_defined_literals != 200809"
#endif

#ifndef __cpp_lambdas
#  error "__cpp_lambdas"
#elif __cpp_lambdas != 200907
#  error "__cpp_lambdas != 200907"
#endif

#ifndef __cpp_constexpr
#  error "__cpp_constexpr"
#elif __cpp_constexpr != 200704
#  error "__cpp_constexpr != 200704"
#endif

#ifndef __cpp_static_assert
#  error "__cpp_static_assert"
#elif __cpp_static_assert != 200410
#  error "__cpp_static_assert != 200410"
#endif

#ifndef __cpp_decltype
#  error "__cpp_decltype"
#elif __cpp_decltype != 200707
#  error "__cpp_decltype != 200707"
#endif

#ifndef __cpp_attributes
#  error "__cpp_attributes"
#elif __cpp_attributes != 200809
#  error "__cpp_attributes != 200809"
#endif

#ifndef __cpp_rvalue_reference
#  error "__cpp_rvalue_reference"
#elif __cpp_rvalue_reference != 200610
#  error "__cpp_rvalue_reference != 200610"
#endif

#ifndef __cpp_variadic_templates
#  error "__cpp_variadic_templates"
#elif __cpp_variadic_templates != 200704
#  error "__cpp_variadic_templates != 200704"
#endif

#ifndef __cpp_alias_templates
#  error "__cpp_alias_templates"
#elif __cpp_alias_templates != 200704
#  error "__cpp_alias_templates != 200704"
#endif

// Begin C++14 tests.

#ifndef __cpp_binary_literals
#  error "__cpp_binary_literals"
#elif __cpp_binary_literals != 201304
#  error "__cpp_binary_literals != 201304"
#endif

#ifndef __cpp_init_captures
#  error "__cpp_init_captures"
#elif __cpp_init_captures != 201304
#  error "__cpp_init_captures != 201304"
#endif

#ifndef __cpp_generic_lambdas
#  error "__cpp_generic_lambdas"
#elif __cpp_generic_lambdas != 201304
#  error "__cpp_generic_lambdas != 201304"
#endif

//  TODO: Change 200704 to 201304 when C++14 constexpr goes in.
#ifndef __cpp_constexpr
#  error "__cpp_constexpr"
#elif __cpp_constexpr != 200704
#  error "__cpp_constexpr != 200704"
#endif

#ifndef __cpp_decltype_auto
#  error "__cpp_decltype_auto"
#elif __cpp_decltype_auto != 201304
#  error "__cpp_decltype_auto != 201304"
#endif

#ifndef __cpp_return_type_deduction
#  error "__cpp_return_type_deduction"
#elif __cpp_return_type_deduction != 201304
#  error "__cpp_return_type_deduction != 201304"
#endif

#ifndef __cpp_runtime_arrays
#  error "__cpp_runtime_arrays"
#elif __cpp_runtime_arrays != 201304
#  error "__cpp_runtime_arrays != 201304"
#endif

//  Aggregate initializers not in yet.
#ifdef __cpp_aggregate_nsdmi
#  error "__cpp_aggregate_nsdmi"
#endif

//  Variable templates not in yet.
#ifdef __cpp_variable_templates
#  error "__cpp_variable_templates"
#endif

#ifndef __cpp_digit_separators
#  error "__cpp_digit_separators"
#elif __cpp_digit_separators != 201309
#  error "__cpp_digit_separators != 201309"
#endif

#ifndef __cpp_attribute_deprecated
#  error "__cpp_attribute_deprecated"
#elif __cpp_attribute_deprecated != 201309
#  error "__cpp_attribute_deprecated != 201309"
#endif

//  Sized deallocation not in yet.
#ifdef __cpp_sized_deallocation
#  error "__cpp_sized_deallocation"
#endif

// Begin include checks.

//  Check for __has_include macro.
#ifndef __has_include
#  error "__has_include"
#endif

//  Quoted complex.h should find at least the bracket version (use operator).
#if __has_include__ "complex.h"
#else
#  error "complex.h"
#endif

//  Try known bracket header (use operator).
#if __has_include__(<complex>)
#else
#  error "<complex>"
#endif

//  Define and use a macro to invoke the operator.
#define sluggo(TXT) __has_include__(TXT)

#if sluggo(<complex>)
#else
#  error "<complex>"
#endif

#if ! sluggo(<complex>)
#  error "<complex>"
#else
#endif

//  Quoted complex.h should find at least the bracket version.
#if __has_include("complex.h")
#else
#  error "complex.h"
#endif

//  Try known local quote header.
#if __has_include("complex_literals.h")
#else
#  error "\"complex_literals.h\""
#endif

//  Try nonexistent bracket header.
#if __has_include(<stuff>)
#  error "<stuff>"
#else
#endif

//  Try nonexistent quote header.
#if __has_include("phlegm")
#  error "\"phlegm\""
#else
#endif

//  Test __has_include_next.
#if __has_include("phoobhar.h")
#  include "phoobhar.h"
#else
#  error "__has_include(\"phoobhar.h\")"
#endif

//  Try a macro.
#define COMPLEX_INC "complex.h"
#if __has_include(COMPLEX_INC)
#else
#  error COMPLEX_INC
#endif

//  Realistic use of __has_include.
#if __has_include(<array>)
#  define STD_ARRAY 1
#  include <array>
  template<typename _Tp, size_t _Num>
    using array = std::array<_Tp, _Num>;
#elif __has_include(<tr1/array>)
#  define TR1_ARRAY 1
#  include <tr1/array>
  template<typename _Tp, size_t _Num>
    typedef std::tr1::array<_Tp, _Num> array;
#endif
