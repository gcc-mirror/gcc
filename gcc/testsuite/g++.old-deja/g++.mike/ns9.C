// { dg-do run  }
namespace Foo {
  int bar() {
    return 0;
  }
}

using Foo::bar;

int main() {
  return bar();
}
