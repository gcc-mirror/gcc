// excess errors test - XFAIL *-*-*
namespace Foo {
  bar() {
    return 0;
  }
}

main() {
  return Foo::bar();
}
