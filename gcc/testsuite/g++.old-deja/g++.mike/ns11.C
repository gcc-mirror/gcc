class Foo {
};

namespace A {
  namespace Foo {
    int bar() {
      return 0;
    }
  }

  int mymain() {
    return Foo::bar();
  }
}

int main() {
  return A::mymain();
}
