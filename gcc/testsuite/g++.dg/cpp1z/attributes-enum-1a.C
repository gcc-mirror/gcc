// { dg-do compile { target c++14_down } }
// This macro should not be defined without c++17.

#ifdef __cpp_enumerator_attributes
#error __cpp_enumerator_attributes defined
#endif
