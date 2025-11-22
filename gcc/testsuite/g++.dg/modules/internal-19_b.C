// PR c++/122636
// { dg-additional-options "-fmodules -Werror=expose-global-module-tu-local" }

import M;
namespace {
  struct Bar {};
  void bar() { Foo<Bar>::foo(); }
}

int main() {
  bar();

  enum class E { };
  Type<E> t;
  t.test(0);
}
