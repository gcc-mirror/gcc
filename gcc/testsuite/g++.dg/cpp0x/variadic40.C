// { dg-options "-std=gnu++11" }
template<typename... Values, typename T>
struct backward_tuple {}; // { dg-error "end" }
