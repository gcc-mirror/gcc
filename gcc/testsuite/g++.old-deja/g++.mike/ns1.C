namespace Foo {
  int bar() {
    return 0;
  }
}

using namespace Foo;

int main() {
  bar();
}
