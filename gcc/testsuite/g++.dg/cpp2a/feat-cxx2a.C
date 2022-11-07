// { dg-options "-std=c++2a -I${srcdir}/g++.dg/cpp1y -I${srcdir}/g++.dg/cpp1y/testinc" }

//  C++98 features:

#ifndef __cpp_rtti
#  error "__cpp_rtti"
#elif  __cpp_rtti != 199711
#  error "__cpp_rtti != 199711"
#endif

#ifndef __cpp_exceptions
#  error "__cpp_exceptions"
#elif  __cpp_exceptions != 199711
#  error "__cpp_exceptions != 199711"
#endif

//  C++11 features:

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

#ifndef __cpp_range_based_for
#  error "__cpp_range_based_for"
#elif __cpp_range_based_for != 201603
#  error "__cpp_range_based_for != 201603"
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

#ifndef __cpp_rvalue_references
#  error "__cpp_rvalue_references"
#elif __cpp_rvalue_references != 200610
#  error "__cpp_rvalue_references != 200610"
#endif

#ifndef __cpp_variadic_templates
#  error "__cpp_variadic_templates"
#elif __cpp_variadic_templates != 200704
#  error "__cpp_variadic_templates != 200704"
#endif

#ifndef __cpp_initializer_lists
#  error "__cpp_initializer_lists"
#elif __cpp_initializer_lists != 200806
#  error "__cpp_initializer_lists != 200806"
#endif

#ifndef __cpp_delegating_constructors
#  error "__cpp_delegating_constructors"
#elif __cpp_delegating_constructors != 200604
#  error "__cpp_delegating_constructors != 200604"
#endif

#ifndef __cpp_nsdmi
#  error "__cpp_nsdmi"
#elif __cpp_nsdmi != 200809
#  error "__cpp_nsdmi != 200809"
#endif

#ifndef __cpp_inheriting_constructors
#  error "__cpp_inheriting_constructors"
#elif  __cpp_inheriting_constructors!= 201511
#  error "__cpp_inheriting_constructors != 201511"
#endif

#ifndef __cpp_ref_qualifiers
#  error "__cpp_ref_qualifiers"
#elif __cpp_ref_qualifiers != 200710
#  error "__cpp_ref_qualifiers != 200710"
#endif

#ifndef __cpp_alias_templates
#  error "__cpp_alias_templates"
#elif __cpp_alias_templates != 200704
#  error "__cpp_alias_templates != 200704"
#endif

#ifndef __cpp_threadsafe_static_init
#  error "__cpp_threadsafe_static_init"
#elif __cpp_threadsafe_static_init != 200806
#  error "__cpp_threadsafe_static_init != 200806"
#endif

//  C++14 features:

#ifndef __cpp_binary_literals
#  error "__cpp_binary_literals"
#elif __cpp_binary_literals != 201304
#  error "__cpp_binary_literals != 201304"
#endif

#ifndef __cpp_init_captures
#  error "__cpp_init_captures"
#elif __cpp_init_captures != 201803
#  error "__cpp_init_captures != 201803"
#endif

#ifndef __cpp_generic_lambdas
#  error "__cpp_generic_lambdas"
#elif __cpp_generic_lambdas != 201707
#  error "__cpp_generic_lambdas != 201707"
#endif

#ifndef __cpp_constexpr
#  error "__cpp_constexpr"
#elif __cpp_constexpr != 202002
#  error "__cpp_constexpr != 202002"
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

#ifndef __cpp_aggregate_nsdmi
#  error "__cpp_aggregate_nsdmi"
#elif __cpp_aggregate_nsdmi != 201304
#  error "__cpp_aggregate_nsdmi != 201304"
#endif

#ifndef __cpp_variable_templates
#  error "__cpp_variable_templates"
#elif __cpp_variable_templates != 201304
#  error "__cpp_variable_templates != 201304"
#endif

#ifndef __cpp_digit_separators
#  error "__cpp_digit_separators"
#elif __cpp_digit_separators != 201309
#  error "__cpp_digit_separators != 201309"
#endif

#ifndef __cpp_sized_deallocation
#  error "__cpp_sized_deallocation"
#elif __cpp_sized_deallocation != 201309
#  error "__cpp_sized_deallocation != 201309"
#endif

//  GNU VLA support:

#ifndef __cpp_runtime_arrays
#  error "__cpp_runtime_arrays"
#elif __cpp_runtime_arrays != 198712
#  error "__cpp_runtime_arrays != 198712"
#endif

//  C++11 attributes:

#ifdef __has_cpp_attribute
#  if ! __has_cpp_attribute(noreturn)
#    error "__has_cpp_attribute(noreturn)"
#  elif __has_cpp_attribute(noreturn) != 200809
#    error "__has_cpp_attribute(noreturn) != 200809"
#  endif
#else
#  error "__has_cpp_attribute"
#endif

//  Attribute carries_dependency not in yet.
//#ifdef __has_cpp_attribute
//#  if ! __has_cpp_attribute(carries_dependency)
//#    error "__has_cpp_attribute(carries_dependency)"
//#  elif __has_cpp_attribute(carries_dependency) != 200809
//#    error "__has_cpp_attribute(carries_dependency) != 200809"
//#  endif
//#else
//#  error "__has_cpp_attribute"
//#endif

//  C++14 attributes:

#ifdef __has_cpp_attribute
#  if ! __has_cpp_attribute(deprecated)
#    error "__has_cpp_attribute(deprecated)"
#  elif __has_cpp_attribute(deprecated) != 201309
#    error "__has_cpp_attribute(deprecated) != 201309"
#  endif
#else
#  error "__has_cpp_attribute"
#endif

//  Include checks:

//  Check for __has_include macro.
#ifndef __has_include
#  error "__has_include"
#endif

//  Try known bracket header (use operator).
#if __has_include (<complex>)
#else
#  error "<complex>"
#endif

//  Define and use a macro to invoke the operator.
#define sluggo(TXT) __has_include(TXT)

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
  template<typename _Tp, std::size_t _Num>
    using array = std::array<_Tp, _Num>;
#elif __has_include(<tr1/array>)
#  define TR1_ARRAY 1
#  include <tr1/array>
  template<typename _Tp, std::size_t _Num>
    typedef std::tr1::array<_Tp, _Num> array;
#endif

// C++17 features:

#ifndef __cpp_unicode_characters
#  error "__cpp_unicode_characters"
#elif __cpp_unicode_characters != 201411
#  error "__cpp_unicode_characters != 201411"
#endif

#ifndef __cpp_static_assert
#  error "__cpp_static_assert"
#elif __cpp_static_assert != 201411
#  error "__cpp_static_assert != 201411"
#endif

#ifndef __cpp_namespace_attributes
#  error "__cpp_namespace_attributes"
#elif __cpp_namespace_attributes != 201411
#  error "__cpp_namespace_attributes != 201411"
#endif

#ifndef __cpp_enumerator_attributes
#  error "__cpp_enumerator_attributes"
#elif __cpp_enumerator_attributes != 201411
#  error "__cpp_enumerator_attributes != 201411"
#endif

#ifndef __cpp_nested_namespace_definitions
#  error "__cpp_nested_namespace_definitions"
#elif __cpp_nested_namespace_definitions != 201411
#  error "__cpp_nested_namespace_definitions != 201411"
#endif

#ifndef __cpp_fold_expressions
#  error "__cpp_fold_expressions"
#elif __cpp_fold_expressions != 201603
#  error "__cpp_fold_expressions != 201603"
#endif

#ifndef __cpp_nontype_template_args
#  error "__cpp_nontype_template_args"
#elif __cpp_nontype_template_args != 201911
#  error "__cpp_nontype_template_args != 201911"
#endif

#ifndef __cpp_hex_float
#  error "__cpp_hex_float"
#elif __cpp_hex_float != 201603
#  error "__cpp_hex_float != 201603"
#endif

#ifndef __cpp_aggregate_bases
#  error "__cpp_aggregate_bases"
#elif __cpp_aggregate_bases != 201603
#  error "__cpp_aggregate_bases != 201603"
#endif

#ifndef __cpp_deduction_guides
#  error "__cpp_deduction_guides"
#elif __cpp_deduction_guides != 201907
#  error "__cpp_deduction_guides != 201907"
#endif

#ifndef __cpp_if_constexpr
#  error "__cpp_if_constexpr"
#elif __cpp_if_constexpr != 201606
#  error "__cpp_if_constexpr != 201606"
#endif

#ifndef __cpp_aligned_new
#  error "__cpp_aligned_new"
#elif __cpp_aligned_new != 201606
#  error "__cpp_aligned_new != 201606"
#endif

#ifndef __cpp_template_auto
#  error "__cpp_template_auto"
#elif __cpp_template_auto != 201606
#  error "__cpp_template_auto != 201606"
#endif

#ifndef __cpp_inline_variables
#  error "__cpp_inline_variables"
#elif __cpp_inline_variables != 201606
#  error "__cpp_inline_variables != 201606"
#endif

#ifndef __cpp_capture_star_this
#  error "__cpp_capture_star_this"
#elif __cpp_capture_star_this != 201603
#  error "__cpp_capture_star_this != 201603"
#endif

#ifndef __cpp_noexcept_function_type
#  error "__cpp_noexcept_function_type"
#elif __cpp_noexcept_function_type != 201510
#  error "__cpp_noexcept_function_type != 201510"
#endif

#ifndef __cpp_structured_bindings
#  error "__cpp_structured_bindings"
#elif __cpp_structured_bindings != 201606
#  error "__cpp_structured_bindings != 201606"
#endif

#ifndef __cpp_template_template_args
#  error "__cpp_template_template_args"
#elif __cpp_template_template_args != 201611
#  error "__cpp_template_template_args != 201611"
#endif

#ifndef __cpp_variadic_using
#  error "__cpp_variadic_using"
#elif __cpp_variadic_using != 201611
#  error "__cpp_variadic_using != 201611"
#endif

#ifndef __cpp_guaranteed_copy_elision
#  error "__cpp_guaranteed_copy_elision"
#elif __cpp_guaranteed_copy_elision != 201606
#  error "__cpp_guaranteed_copy_elision != 201606"
#endif

#ifndef __cpp_nontype_template_parameter_auto
#  error "__cpp_nontype_template_parameter_auto"
#elif __cpp_nontype_template_parameter_auto != 201606
#  error "__cpp_nontype_template_parameter_auto != 201606"
#endif

// C++20 features:

#ifndef __cpp_conditional_explicit
#  error "__cpp_conditional_explicit"
#elif __cpp_conditional_explicit != 201806
#  error "__cpp_conditional_explicit != 201806"
#endif

#ifndef __cpp_nontype_template_parameter_class
#  error "__cpp_nontype_template_parameter_class"
#elif __cpp_nontype_template_parameter_class != 201806
#  error "__cpp_nontype_template_parameter_class != 201806"
#endif

#ifndef __cpp_impl_destroying_delete
#  error "__cpp_impl_destroying_delete"
#elif __cpp_impl_destroying_delete != 201806
#  error "__cpp_impl_destroying_delete != 201806"
#endif

#ifndef __cpp_constinit
#  error "__cpp_constinit"
#elif __cpp_constinit != 201907
#  error "__cpp_constinit != 201907"
#endif

#ifndef __cpp_constexpr_dynamic_alloc
#  error "__cpp_constexpr_dynamic_alloc"
#elif __cpp_constexpr_dynamic_alloc != 201907
#  error "__cpp_constexpr_dynamic_alloc != 201907"
#endif

#ifndef __cpp_aggregate_paren_init
#  error "__cpp_aggregate_paren_init"
#elif __cpp_aggregate_paren_init != 201902
#  error "__cpp_aggregate_paren_init != 201902"
#endif

#ifndef __cpp_char8_t
#  error "__cpp_char8_t"
#elif __cpp_char8_t != 202207
#  error "__cpp_char8_t != 202207"
#endif

#ifndef __cpp_designated_initializers
#  error "__cpp_designated_initializers"
#elif __cpp_designated_initializers != 201707
#  error "__cpp_designated_initializers != 201707"
#endif

#ifndef __cpp_constexpr_in_decltype
#  error "__cpp_constexpr_in_decltype"
#elif __cpp_constexpr_in_decltype != 201711
#  error "__cpp_constexpr_in_decltype != 201711"
#endif

#ifndef __cpp_consteval
#  error "__cpp_consteval"
#elif __cpp_consteval != 201811
#  error "__cpp_consteval != 201811"
#endif

#ifndef __cpp_concepts
#  error "__cpp_concepts"
#elif __cpp_concepts != 202002
#  error "__cpp_concepts != 202002"
#endif

#ifndef __cpp_using_enum
#  error "__cpp_using_enum"
#elif __cpp_using_enum != 201907
#  error "__cpp_using_enum != 201907"
#endif

// C++20 attributes:

#ifdef __has_cpp_attribute

#  if ! __has_cpp_attribute(maybe_unused)
#    error "__has_cpp_attribute(maybe_unused)"
#  elif __has_cpp_attribute(maybe_unused) != 201603
#    error "__has_cpp_attribute(maybe_unused) != 201603"
#  endif

#  if ! __has_cpp_attribute(nodiscard)
#    error "__has_cpp_attribute(nodiscard)"
#  elif __has_cpp_attribute(nodiscard) != 201907
#    error "__has_cpp_attribute(nodiscard) != 201907"
#  endif

#  if ! __has_cpp_attribute(fallthrough)
#    error "__has_cpp_attribute(fallthrough)"
#  elif __has_cpp_attribute(fallthrough) != 201603
#    error "__has_cpp_attribute(fallthrough) != 201603"
#  endif

#  if ! __has_cpp_attribute(no_unique_address)
#    error "__has_cpp_attribute(no_unique_address)"
#  elif __has_cpp_attribute(no_unique_address) != 201803
#    error "__has_cpp_attribute(no_unique_address) != 201803"
#  endif

#  if ! __has_cpp_attribute(likely)
#    error "__has_cpp_attribute(likely)"
#  elif __has_cpp_attribute(likely) != 201803
#    error "__has_cpp_attribute(likely) != 201803"
#  endif

#  if ! __has_cpp_attribute(unlikely)
#    error "__has_cpp_attribute(unlikely)"
#  elif __has_cpp_attribute(unlikely) != 201803
#    error "__has_cpp_attribute(unlikely) != 201803"
#  endif

#else
#  error "__has_cpp_attribute"
#endif
