// https://issues.dlang.org/show_bug.cgi?id=21039

/*
TEST_OUTPUT:
---
runnable/test21039.d(14): Deprecation: alias this for classes/interfaces is deprecated
---
*/

class Inner {}

class Outer {
    Inner inner;
    alias inner this;
    this(Inner i) { inner = i; }
}

void main() {
  auto inner = new Inner;
  auto outer = new Outer(new Inner);

  // implicit cast goes through 'alias this'

  Inner inner1 = outer;  // Already does it
  assert(inner1);

  Inner[] inners = [inner, outer]; // Fixed

  assert(inners[0], "first element is null");
  assert(inners[1], "second element is null");

  Inner inner2 = 1 ? outer : inner; // Fixed
  assert(inner2);
}
