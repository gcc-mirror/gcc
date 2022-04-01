// PR c++/102869
// { dg-do compile { target c++11 } }

template<int...> struct integer_sequence;

template<int _Num>
using make_index_sequence = integer_sequence<__integer_pack(_Num)...>;

template<class...> struct Tuple;

template<int... Is> using tuple_t = Tuple<make_index_sequence<Is>...>;

template<int... Is>
void f() {
  tuple_t<Is...> t;
}
