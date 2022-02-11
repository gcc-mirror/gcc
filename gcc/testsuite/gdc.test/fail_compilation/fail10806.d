/*
TEST_OUTPUT:
---
fail_compilation/fail10806.d(12): Error: function `fail10806.Class.clone` incompatible covariant types `First()` and `Second()`
---
*/

interface First { First clone(); }
interface Second { Second clone(); void call(); }

class Class : First, Second {
  override Class clone() { return this; }
  override void call() { }
}

void main() {
  (cast(Second) new Class).clone().call();
}
