// { dg-do run  }
namespace A{
 void foo(int){}
}
namespace B{
  void foo(bool){}
}

void bar()
{
  using B::foo;
  using A::foo;
  foo(true);
}

namespace Foo {
  template<class N> void Hello(N) {}
}

int main() {
  using Foo::Hello;
  Hello(4);
  bar();
}
