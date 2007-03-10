// { dg-options "-std=gnu++0x" }
template<typename... Values, typename T>
struct backward_tuple {}; // { dg-error "end" }
