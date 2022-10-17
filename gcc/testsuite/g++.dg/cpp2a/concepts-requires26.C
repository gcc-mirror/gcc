// PR c++/101181
// { dg-do compile { target c++20 } }

template<class T,
	 bool = requires { typename T::type; }>
struct p { using type = void; };

template<class T>
struct p<T, true> { using type = typename T::type; };

template<class T> using P = typename p<T>::type;

using type1 = P<int>;
using type1 = void;

struct A { using type = char; };
using type2 = P<A>;
using type2 = char;
