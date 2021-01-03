// { dg-additional-options "-fmodules-ts -std=c++20" }

export module M;
// { dg-module-cmi M }

export template <typename T0>
struct traits
{
  static constexpr int variant = 0;
};

// #2
template <typename T2>
requires requires { typename T2::element_type; }
struct traits<T2 *>
{
  using type = typename T2::element_type;
  static constexpr int variant = 2;
};


// #1
template <typename T1>
struct traits<T1 *>
{
  using type = T1;
  static constexpr int variant = 1;
};


// #3
template <typename T3>
requires requires { typename T3::value_type; }
struct traits<T3 *>
{
  using type = typename T3::value_type;
  static constexpr int variant = 3;
};
