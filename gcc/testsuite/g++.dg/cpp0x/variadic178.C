// PR c++/92068
// { dg-do compile { target c++11 } }

template <typename, typename> struct a;
template <typename b, typename c, typename... d>
struct a<b, c, d...> { };	// { dg-error "arguments" }
