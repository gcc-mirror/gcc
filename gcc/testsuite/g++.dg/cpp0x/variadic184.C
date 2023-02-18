// PR c++/102870
// { dg-do compile { target c++11 } }

template <typename> struct integer_sequence;
template <typename _Tp, _Tp _Num>
using make_integer_sequence = integer_sequence<_Tp, __integer_pack(_Num)...>;
template <long _Num>
using make_index_sequence = make_integer_sequence<long, _Num>;
template <class> struct Tuple;
template <int... Is> using type = Tuple<make_index_sequence<Is>...>;
template <int... Is> void f() { Tuple<type<Is>>{}; } // { dg-error "parameter packs not expanded" }
int main() { f(); }
