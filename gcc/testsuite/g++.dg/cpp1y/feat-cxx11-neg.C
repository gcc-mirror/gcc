// { dg-do compile { target c++11_only } }

// C++14 features:

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


//  Array TS features:

#ifndef __cpp_runtime_arrays
#  error "__cpp_runtime_arrays" // { dg-error "error" }
#endif

//  C++14 attributes:

//  Attribute [[deprecated]] is allowed in C++11 as an extension.
//#ifdef __has_cpp_attribute
//#  if __has_cpp_attribute(deprecated) == 201309
//#    error "__has_cpp_attribute(deprecated)" // {  }
//#  endif
//#else
//#  error "__has_cpp_attribute"
//#endif
