// PR c++/77446
// { dg-do compile { target c++14 } }

struct inner {
  int  field_a = 0;
  bool field_b = false;

  explicit constexpr inner(const int &arg_field_a) noexcept
    : field_a{ arg_field_a } {}
};

struct outer {
  inner the_inner = inner{ 0 };

  constexpr outer() noexcept = default;
  constexpr int set_inner(const inner &arg_inner) {
    the_inner = arg_inner;
    return 0;
  }
};

constexpr inner another_inner{ 1 };
static_assert( outer{}.set_inner( another_inner ) == 0,  "" );
