// PR c++/79380
// { dg-do compile { target c++11 } }

template < typename > constexpr int f () {  return 8;  }

// should have been: struct alignas (f<int>()) S {};
struct alignas (f) S {};  // { dg-error "17:'alignas' argument has non-integral type" }
