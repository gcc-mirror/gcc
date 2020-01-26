// PR c++/92601
// { dg-additional-options "-g -fchecking -std=c++17" }

typedef int size_t;
template <typename, int __v> struct integral_constant {
  static constexpr int value = __v;
};
template <typename> struct A;
template <typename _Tp> using __remove_cv_t = typename A<_Tp>::type;
template <typename _Tp, typename _Up>
struct B : integral_constant<bool, __is_same_as(_Tp, _Up)> {};
template <typename...> class tuple;
template <typename> struct A {
  using type = tuple<const char *, const char *>;
};
template <typename> struct C { typedef __remove_cv_t<int> __type; };
template <typename _Tp> class D {
public:
  typedef typename C<_Tp>::__type type;
};
template <bool> struct enable_if;
template <int> struct F {};
template <typename, typename> class G {
public:
  int operator*();
  void operator++();
};
template <typename _Iterator, typename _Container>
bool operator!=(G<_Iterator, _Container>, G<_Iterator, _Container>);
template <typename> class H;
template <typename = H<tuple<const char *, const char *>>> class vector {
public:
  typedef G<int, vector> iterator;
  iterator begin();
  iterator end();
};
template <typename> struct pack_c { typedef pack_c type; };
template <typename, typename> struct make_index_pack_join;
template <size_t... Left, size_t... Right>
struct make_index_pack_join<pack_c<size_t, Left...>, pack_c<size_t, Right...>>
    : pack_c<size_t> {};
template <int N>
struct I
    : make_index_pack_join<typename I<N / 2>::type, typename I<N / 2>::type> {};
template <> struct I<1> : pack_c<size_t> {};
template <typename TTuple, typename>
struct are_tuples_compatible_not_same
    : F<B<typename D<TTuple>::type, int>::value> {};
template <typename...> struct tuple_impl;
template <size_t... Is, typename... Ts>
struct tuple_impl<pack_c<size_t, Is...>, Ts...> {
  template <typename UTuple, typename enable_if<are_tuples_compatible_not_same<
                                 tuple<>, UTuple>::value>::type>
  tuple_impl(UTuple &&);
};
template <typename... Ts> class tuple {
  tuple_impl<typename I<sizeof...(Ts)>::type> _impl;
  tuple(tuple &) = default;
};
vector message_handler_registrations;
void fn1() {
  for (auto t : message_handler_registrations)
    ;
}
