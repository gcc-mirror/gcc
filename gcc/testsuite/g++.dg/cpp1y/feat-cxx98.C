// { dg-do compile { target c++98_only } }
// { dg-options "" }

//  C++98 features:

#ifndef __cpp_rtti
#  error "__cpp_rtti"
#elif  __cpp_rtti != 199711
#  error "__cpp_rtti != 199711"
#endif

#ifndef __cpp_exceptions
#  error "__cpp_exceptions"
#elif  __cpp_exceptions != 199711
#  error "__cpp_exceptions != 199711"
#endif

//  C++11 features allowed in C++98:

#ifndef __cpp_threadsafe_static_init
#  error "__cpp_threadsafe_static_init"
#elif __cpp_threadsafe_static_init != 200806
#  error "__cpp_threadsafe_static_init != 200806"
#endif

//  C++14 features allowed in C++98 in non-ANSI modes:

#ifndef __cpp_binary_literals
#  error "__cpp_binary_literals"
#elif  __cpp_binary_literals != 201304
#  error "__cpp_binary_literals != 201304"
#endif

//  GNU VLA support:

#ifndef __cpp_runtime_arrays
#  error "__cpp_runtime_arrays"
#elif __cpp_runtime_arrays != 198712
#  error "__cpp_runtime_arrays != 198712"
#endif
