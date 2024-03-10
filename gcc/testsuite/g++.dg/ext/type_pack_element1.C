// { dg-do compile { target c++11 } }

using ty0 = __type_pack_element<0, int>;
using ty0 = __type_pack_element<0, int, char>;
using ty0 = int;

using ty1 = __type_pack_element<1, int, char>;
using ty1 = __type_pack_element<(6 - 5) * 1, int, char>;
using ty1 = char;

template<int N, class... Ts>
using __const_type_pack_element_t = const __type_pack_element<N, Ts...>;

using ty2 = __const_type_pack_element_t<2, int, char, long>;
using ty2 = const long;

template<class T> struct A { };
using ty3 = __type_pack_element<3, int, int, int, A<int>>;
using ty3 = A<int>;
