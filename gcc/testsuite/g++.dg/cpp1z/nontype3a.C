// { dg-do compile { target c++14_down } }
// This macro should not be defined without c++17.

#ifdef __cpp_nontype_template_args
#error __cpp_nontype_template_args defined
#endif
