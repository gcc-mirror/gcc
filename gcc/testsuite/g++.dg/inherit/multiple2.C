// PR c++/88848
// { dg-do compile { target c++17 } }

template<typename>
struct True { static constexpr bool value{ true }; };

template<int VALUE>
struct Integer { static constexpr int value{ VALUE }; };

template<int VALUE, typename TYPE>
struct Foo
{
  using Integer_t = Integer<VALUE>;

  static TYPE get_type(Integer_t);
};

template<typename... ARGS>
struct Bar : ARGS...
{
  using ARGS::get_type...;

  template<int VALUE>
  using Type_t = decltype(get_type(Integer<VALUE>{}));

  Bar() { static_assert((True< Type_t<ARGS::Integer_t::value> >::value && ...)); }

  static_assert((True< Type_t<ARGS::Integer_t::value> >::value && ...));
};

int main()
{
  Bar<Foo<4, float>, Foo<8, double>> obj;
  return int{ sizeof(obj) };
}
