namespace Foo {
  bar() {
    return 0;
  }
}

using Foo::bar;

main() {
  return bar();
}
