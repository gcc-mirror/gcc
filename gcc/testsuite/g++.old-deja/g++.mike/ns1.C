namespace Foo { // WARNING - namespaces mostly broken
  bar() {
    return 0;
  }
}

using namespace Foo; // ERROR - using not implemented

main() {
  bar();
}
