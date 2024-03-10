// { dg-do compile { target c++11 } }

int p;

using type = __type_pack_element<&p, int>;      // { dg-error "not an integral constant" }
using type = __type_pack_element<1, int>;       // { dg-error "out of range" }
using type = __type_pack_element<2, int, char>; // { dg-error "out of range" }
using type = __type_pack_element<-1, int>;      // { dg-error "negative" }

template<int N, class... Ts>
using __type_pack_element_t = __type_pack_element<N, Ts...>;
// { dg-error "out of range" "" { target *-*-* } .-1 }

using type = __type_pack_element_t<3, int, char, long>; // { dg-message "here" }
