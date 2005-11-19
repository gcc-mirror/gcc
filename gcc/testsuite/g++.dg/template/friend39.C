// PR c++/8355

namespace Foo { template <typename T> void foo();}
struct Bar
{
  friend void Foo::foo<int>();
};
