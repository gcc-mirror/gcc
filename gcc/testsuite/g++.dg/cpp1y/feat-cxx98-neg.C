// { dg-do compile { target c++98_only } }

//  C++11 features:

#ifndef __cpp_runtime_arrays
#  error "__cpp_runtime_arrays" // { dg-error "error" }
#endif

#ifndef __cpp_unicode_characters
#  error "__cpp_unicode_characters" // { dg-error "error" }
#endif

#ifndef __cpp_raw_strings
#  error "__cpp_raw_strings" // { dg-error "error" }
#endif

#ifndef __cpp_unicode_literals
#  error "__cpp_unicode_literals" // { dg-error "error" }
#endif

#ifndef __cpp_user_defined_literals
#  error "__cpp_user_defined_literals" // { dg-error "error" }
#endif

#ifndef __cpp_lambdas
#  error "__cpp_lambdas" // { dg-error "error" }
#endif

#ifndef __cpp_constexpr
#  error "__cpp_constexpr" // { dg-error "error" }
#endif

#ifndef __cpp_static_assert
#  error "__cpp_static_assert" // { dg-error "error" }
#endif

#ifndef __cpp_decltype
#  error "__cpp_decltype" // { dg-error "error" }
#endif

#ifndef __cpp_attributes
#  error "__cpp_attributes" // { dg-error "error" }
#endif

#ifndef __cpp_rvalue_references
#  error "__cpp_rvalue_references" // { dg-error "error" }
#endif

#ifndef __cpp_variadic_templates
#  error "__cpp_variadic_templates" // { dg-error "error" }
#endif

#ifndef __cpp_initializer_lists
#  error "__cpp_initializer_lists" // { dg-error "error" }
#endif

#ifndef __cpp_delegating_constructors
#  error "__cpp_delegating_constructors" // { dg-error "error" }
#endif

#ifndef __cpp_nsdmi
#  error "__cpp_nsdmi" // { dg-error "error" }
#endif

#ifndef __cpp_inheriting_constructors
#  error "__cpp_inheriting_constructors" // { dg-error "error" }
#endif

#ifndef __cpp_ref_qualifiers
#  error "__cpp_ref_qualifiers" // { dg-error "error" }
#endif

#ifndef __cpp_alias_templates
#  error "__cpp_alias_templates" // { dg-error "error" }
#endif

// C++14 features:

// C++98 gets binary literals in non-ANSI modes.
//#ifndef __cpp_binary_literals
//#  error "__cpp_binary_literals"
//#endif

#ifndef __cpp_init_captures
#  error "__cpp_init_captures" // { dg-error "error" }
#endif

#ifndef __cpp_generic_lambdas
#  error "__cpp_generic_lambdas" // { dg-error "error" }
#endif

#ifndef __cpp_decltype_auto
#  error "__cpp_decltype_auto" // { dg-error "error" }
#endif

#ifndef __cpp_return_type_deduction
#  error "__cpp_return_type_deduction" // { dg-error "error" }
#endif

#ifndef __cpp_aggregate_nsdmi
#  error "__cpp_aggregate_nsdmi" // { dg-error "error" }
#endif

#ifndef __cpp_variable_templates
#  error "__cpp_variable_templates" // { dg-error "error" }
#endif

#ifndef __cpp_digit_separators
#  error "__cpp_digit_separators" // { dg-error "error" }
#endif

#ifndef __cpp_sized_deallocation
#  error "__cpp_sized_deallocation" // { dg-error "error" }
#endif

// C++17 features:

#ifndef __cpp_namespace_attributes
#  error "__cpp_namespace_attributes" // { dg-error "error" }
#endif

#ifndef __cpp_nested_namespace_definitions
#  error "__cpp_nested_namespace_definitions" // { dg-error "error" }
#endif

//  C++11 attributes:

#ifdef __has_cpp_attribute
#  if __has_cpp_attribute(noreturn) == 200809
#    error "__has_cpp_attribute(noreturn) == 200809" // { dg-error "error" }
#  endif
#else
#  error "__has_cpp_attribute"
#endif

//  Attribute carries_dependency not in yet.
//#ifdef __has_cpp_attribute
//#  if __has_cpp_attribute(carries_dependency) == 200809
//#    error "__has_cpp_attribute(carries_dependency) == 200809" // {  }
//#  endif
//#else
//#  error "__has_cpp_attribute"
//#endif

//  C++14 attributes:

//  Attribute [[deprecated]] is allowed in C++11 as an extension.
//#ifdef __has_cpp_attribute
//#  if __has_cpp_attribute(deprecated) == 201309
//#    error "__has_cpp_attribute(deprecated)" // {  }
//#  endif
//#else
//#  error "__has_cpp_attribute"
//#endif
