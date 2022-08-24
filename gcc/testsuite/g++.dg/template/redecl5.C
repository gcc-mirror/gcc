// PR c++/106311
// { dg-do compile }

template <typename, long> struct array; // { dg-error "template parameter" }
template <typename, size_t X> struct array { }; // { dg-error "declared" }
