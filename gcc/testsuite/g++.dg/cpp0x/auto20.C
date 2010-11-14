// Test for proper non-deduced context handling of the initializer
// for an auto declaration/new.
// { dg-options -std=c++0x }

struct with_apply
{
  template <unsigned>
  void apply(const double&){}
};

auto p = &with_apply::apply<0>;
auto pp = new auto(&with_apply::apply<0>);

template <class T>
void f()
{
  auto p = &T::template apply<0>;
}

template void f<with_apply>();
