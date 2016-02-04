// { dg-options -std=c++14 }

// C++17 features:

#ifndef __cpp_namespace_attributes
#  error "__cpp_namespace_attributes" // { dg-error "error" }
#endif

#ifndef __cpp_nested_namespace_definitions
#  error "__cpp_nested_namespace_definitions" // { dg-error "error" }
#endif
