// { dg-do compile { target c++11_only } }
// { dg-options "-fsized-deallocation" }

// C++14 features:

#ifndef __cpp_sized_deallocation
#  error "__cpp_sized_deallocation"
#elif __cpp_sized_deallocation != 201309
#  error "__cpp_sized_deallocation != 201309"
#endif
