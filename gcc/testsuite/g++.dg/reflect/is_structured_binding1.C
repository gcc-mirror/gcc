// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
#include <meta>
#include <tuple>

struct S
{
  int a;
  int b;
  double c;
};

int
main ()
{
  {
    // The initializer for the structured binding declaration has array type.
    const int struct_binding[] = { 1, 2, 3 };

    auto [x, y, z] = struct_binding;

    static_assert (std::meta::is_structured_binding (^^x));
    static_assert (std::meta::is_structured_binding (^^y));
    static_assert (std::meta::is_structured_binding (^^z));
  }

  {
    // The initializer for the structured binding declaration is tuple-like.
    auto struct_binding = std::make_tuple (1, 2, 3);

    auto [x, y, z] = struct_binding;

    static_assert (std::meta::is_structured_binding (^^x));
    static_assert (std::meta::is_structured_binding (^^y));
    static_assert (std::meta::is_structured_binding (^^z));
  }

  {
    // The initializer for the structured binding declaration is a struct
    auto struct_binding = S{ 1, 2, 3.14 };
    auto [x, y, z] = struct_binding;

    static_assert (std::meta::is_structured_binding (^^x));
    static_assert (std::meta::is_structured_binding (^^y));
    static_assert (std::meta::is_structured_binding (^^z));
  }

  {
    // const auto structured binding
    const int struct_binding[] = { 10, 20 };
    const auto [x, y] = struct_binding;

    static_assert (std::meta::is_structured_binding (^^x));
    static_assert (std::meta::is_structured_binding (^^y));
  }

  {
    // auto& structured binding
    int struct_binding[] = { 30, 40 };
    auto &[x, y] = struct_binding;

    static_assert (std::meta::is_structured_binding (^^x));
    static_assert (std::meta::is_structured_binding (^^y));
  }

  {
    // const auto& structured binding
    const int struct_binding[] = { 50, 60 };
    const auto &[x, y] = struct_binding;

    static_assert (std::meta::is_structured_binding (^^x));
    static_assert (std::meta::is_structured_binding (^^y));
  }

  {
    // auto&& with rvalue
    auto &&[x, y] = std::make_tuple (90, 100);

    static_assert (std::meta::is_structured_binding (^^x));
    static_assert (std::meta::is_structured_binding (^^y));
  }

  {
    // Negative cases
    int var = 42;
    const int const_var = 43;
    int &ref_var = var;

    S s{ 1, 2, 3.14 };

    static_assert (!std::meta::is_structured_binding (^^var));
    static_assert (!std::meta::is_structured_binding (^^const_var));
    static_assert (!std::meta::is_structured_binding (^^ref_var));
    static_assert (!std::meta::is_structured_binding (^^s));
    static_assert (!std::meta::is_structured_binding (^^S));
    static_assert (!std::meta::is_structured_binding (^^int));
    static_assert (!std::meta::is_structured_binding (^^::));
  }
}
