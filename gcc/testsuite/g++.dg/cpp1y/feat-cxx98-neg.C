// { dg-do compile { target c++98_only } }
// { dg-options "-ansi" }

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

#ifndef __cpp_rvalue_reference
#  error "__cpp_rvalue_reference" // { dg-error "error" }
#endif

#ifndef __cpp_variadic_templates
#  error "__cpp_variadic_templates" // { dg-error "error" }
#endif

#ifndef __cpp_alias_templates
#  error "__cpp_alias_templates" // { dg-error "error" }
#endif

// C++14

// C++98 gets binary literals.
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

//  Aggregate initializers not in yet.
//#ifdef __cpp_aggregate_nsdmi
//#  error "__cpp_aggregate_nsdmi"
//#endif

#ifndef __cpp_variable_templates
#  error "__cpp_variable_templates" // { dg-error "error" }
#endif

#ifndef __cpp_digit_separators
#  error "__cpp_digit_separators" // { dg-error "error" }
#endif

#ifndef __cpp_attribute_deprecated
#  error "__cpp_attribute_deprecated" // { dg-error "error" }
#endif

//  Sized deallocation not in yet.
//#ifdef __cpp_sized_deallocation
//#  error "__cpp_sized_deallocation"
//#endif
