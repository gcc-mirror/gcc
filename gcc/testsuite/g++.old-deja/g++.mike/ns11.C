class Foo {
};

namespace A {
  namespace Foo {
    bar() {
      return 0;
    }
  }

  mymain() {
    return Foo::bar();
  }
}

main() {
  return A::mymain();
}
