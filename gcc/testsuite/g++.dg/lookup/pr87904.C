// PR c++ 87904 ICE failing to initiate deduping

namespace X {
  void Foo (char);
}

struct B {
  friend void Foo (int);
};

using X::Foo;

void Foo (float);
void Foo(int);

void frob ()
{
  using namespace X;

  Foo (1);
}
