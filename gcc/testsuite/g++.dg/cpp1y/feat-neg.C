// { dg-do compile }
// { dg-options "-fno-rtti -fno-exceptions" }

//  C++98 features with explicit opt-out:

#ifndef __cpp_rtti
#  error "__cpp_rtti" // { dg-error "error" }
#endif

#ifndef __cpp_exceptions
#  error "__cpp_exceptions" // { dg-error "error" }
#endif
