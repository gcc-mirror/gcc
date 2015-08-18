// { dg-do compile }
// { dg-options "-std=c++1z" }

#ifndef __cpp_static_assert
#  error "__cpp_static_assert"
#elif __cpp_static_assert != 201411
#  error "__cpp_static_assert != 201411"
#endif
