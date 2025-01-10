// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

struct A {
  int a, b, c;
};

namespace {
A d = { 1, 2, 3 };
auto [a, b, c] = d;
}
