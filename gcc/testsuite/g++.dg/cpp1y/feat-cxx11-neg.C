// { dg-do compile { target c++11_only } }
// { dg-options "-pedantic-errors" }

// These *are* defined in C++14 onwards.

#ifndef __cpp_binary_literals
#  error "__cpp_binary_literals" // { dg-error "error" }
#endif

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

#ifndef __cpp_variable_templates
#  error "__cpp_variable_templates" // { dg-error "error" }
#endif

#ifndef __cpp_digit_separators
#  error "__cpp_digit_separators" // { dg-error "error" }
#endif

#ifndef __cpp_attribute_deprecated
#  error "__cpp_attribute_deprecated" // { dg-error "error" }
#endif

#ifndef __cpp_runtime_arrays
#  error "__cpp_runtime_arrays" // { dg-error "error" }
#endif
