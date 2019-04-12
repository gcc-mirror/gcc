// PR c++/87603
// { dg-do compile { target c++14 } }

template<typename T>
struct basic_string_view
{
  constexpr basic_string_view(T p) noexcept { (void) p.i; }
};

struct X { } x;

bool b = noexcept(basic_string_view<X>{x});
