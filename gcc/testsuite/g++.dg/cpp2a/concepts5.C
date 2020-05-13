// { dg-do compile { target c++20 } }


template <typename T, typename U>
concept same_as = __is_same_as(T, U);

template<typename T>
concept character = same_as<T, char>;

struct T
{
  constexpr T(same_as<int> auto const x) : val(0) { }

  constexpr T(character auto const x) : val(1) { }

  int val;
};

void test()
{
  static_assert(T(0).val == 0);
  static_assert(T('a').val == 1);
}

