// PR c++/101181
// { dg-do compile { target c++20 } }

template<class T,
	 bool = [] () -> bool {
	   if constexpr (requires { typename T::type; })
	     return true;
	   return false;
	 }()>
struct p { using type = void; };

template<class T>
struct p<T, true> { using type = typename T::type; };

template<class T> using P = typename p<T>::type;

using type1 = P<int>;
using type = void;

struct A { using type = char; };
using type2 = P<A>;
using type2 = char;
