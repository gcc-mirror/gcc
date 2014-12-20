// { dg-do compile { target c++14 } }
// { dg-options "-fno-sized-deallocation" }

#ifndef __cpp_sized_deallocation
#  error "__cpp_sized_deallocation" // { dg-error "error" }
#endif
