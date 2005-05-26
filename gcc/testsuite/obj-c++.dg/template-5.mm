// Test that extern template does not get emitted.
// Author: Matt Austern <austern@apple.com>

// { dg-do compile }
// { dg-options "" }
// { dg-final { scan-assembler-not ".globl __ZN3FooIiE5identEi" } }

template <typename X>
struct Foo {
  X ident(X x) { return x; }
};

extern template struct Foo<int>;

int abcde(Foo<int>& foo, int n) {
  return foo.ident(n);
}
