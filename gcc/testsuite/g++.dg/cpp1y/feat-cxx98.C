// { dg-do compile { target c++98_only } }
// { dg-options "" }

//  C++14 features allowed in C++98 in non-ANSI modes:

#ifndef __cpp_binary_literals
#  error "__cpp_binary_literals"
#elif  __cpp_binary_literals != 201304
#  error "__cpp_binary_literals != 201304"
#endif
