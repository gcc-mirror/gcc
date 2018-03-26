// PR c++/85049
// { dg-do compile { target c++11 } }

typedef __SIZE_TYPE__ size_t;
template<typename _Tp, _Tp... _Idx>
struct integer_sequence
{
  typedef _Tp value_type;
  static constexpr size_t size() noexcept { return sizeof...(_Idx); }
};
template<typename _Tp, _Tp _Num>
using make_integer_sequence = integer_sequence<_Tp, __integer_pack(_Num)...>;
template<size_t _Num>
using make_index_sequence = make_integer_sequence<size_t, _Num>;
template<typename... _Types>
using index_sequence_for = make_index_sequence<sizeof...(_Types)>;
template <typename...>
struct tuple {};
template <typename... Ts>
int get(tuple<index_sequence_for<Ts...>, Ts...>);
int x = get(tuple<index_sequence_for<>>{});
