/*
TEST_OUTPUT:
---
fail_compilation/fail11445.d(11): Error: illegal operator `+` for `a` of type `double[string]`
---
*/

void main() {
  double[string] a = [ "foo" : 22.2 ];
  double[string] b = [ "bar" : 22.2 ];
  auto c = a + b;
}
