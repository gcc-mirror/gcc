// PR c++/114349
// { dg-do compile { target c++14 } }

struct B
{
  int i;
};

template <bool BA>
void
goo ()
{
  constexpr bool is_yes = BA;
  struct C
  {
    static auto g(B b) noexcept(is_yes) { }
  };
  C::g({});
}

void
x ()
{
  goo<false>();
}
