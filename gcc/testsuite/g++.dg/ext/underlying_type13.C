// Verify when substituting __underlying_type its cv-quals are carried over.
// { dg-do compile { target c++11 } }

template<class T> using const_underlying_type_t = const __underlying_type(T);
enum A { a };
using type = const_underlying_type_t<A>;
using type = const __underlying_type(A);
