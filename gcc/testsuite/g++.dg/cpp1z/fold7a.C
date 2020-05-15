// { dg-do compile { target c++14_down } }
// This macro should not be defined without c++17.

#ifdef __cpp_fold_expressions
#error __cpp_fold_expressions defined
#endif
