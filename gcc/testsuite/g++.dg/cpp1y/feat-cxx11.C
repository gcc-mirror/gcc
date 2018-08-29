// { dg-do compile }
// { dg-options "-std=gnu++11" }

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

#ifndef __cpp_range_based_for
#  error "__cpp_range_based_for"
#elif __cpp_range_based_for < 200907
#  error "__cpp_range_based_for < 200907"
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

//  C++14 features allowed in C++11 in non-ANSI modes:

#ifndef __cpp_binary_literals
#  error "__cpp_binary_literals"
#elif __cpp_binary_literals != 201304
#  error "__cpp_binary_literals != 201304"
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

#ifdef __has_cpp_attribute
//  Attribute carries_dependency not in yet.
//#  if ! __has_cpp_attribute(carries_dependency)
//#    error "__has_cpp_attribute(carries_dependency)"
//#  elif __has_cpp_attribute(carries_dependency) != 200809
//#    error "__has_cpp_attribute(carries_dependency) != 200809"
//#  endif
#else
#  error "__has_cpp_attribute"
#endif

//  C++14 attributes:

//  Attribute [[deprecated]] is allowed in C++11 as an extension.
#ifdef __has_cpp_attribute
#  if ! __has_cpp_attribute(deprecated)
#    error "__has_cpp_attribute(deprecated)"
#  elif __has_cpp_attribute(deprecated) != 201309
#    error "__has_cpp_attribute(deprecated) != 201309"
#  endif
#else
#  error "__has_cpp_attribute"
#endif
