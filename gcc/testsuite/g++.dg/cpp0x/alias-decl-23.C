// PR c++/52233
// { dg-do compile { target c++11 } }

template <typename t>
struct foo
{
  template <template <typename...> class... xs>
  using type = int;
};

template <typename t, template <typename...> class... xs>
struct bar
{
  using type = typename foo<t>::template type<xs...>;
};

bar<int, foo> x;
