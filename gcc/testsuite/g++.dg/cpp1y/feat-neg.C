// { dg-do compile }
// { dg-options "-fno-rtti -fno-exceptions -fno-threadsafe-statics" }

//  C++98 features with explicit opt-out:

#ifndef __cpp_rtti
#  error "__cpp_rtti" // { dg-error "error" }
#endif

#ifndef __cpp_exceptions
#  error "__cpp_exceptions" // { dg-error "error" }
#endif

//  C++11 features with explicit opt-out:

#ifndef __cpp_threadsafe_static_init
#  error "__cpp_threadsafe_static_init" // { dg-error "error" }
#endif
