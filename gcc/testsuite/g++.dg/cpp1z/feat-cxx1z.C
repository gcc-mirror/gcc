// { dg-do compile }
// { dg-options "-std=c++1z" }

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

#ifndef __cpp_nested_namespace_definitions
#  error "__cpp_nested_namespace_definitions"
#elif __cpp_nested_namespace_definitions != 201411
#  error "__cpp_nested_namespace_definitions != 201411"
#endif
