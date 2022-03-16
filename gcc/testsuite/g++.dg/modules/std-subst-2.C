// { dg-additional-options "-fmodules-ts" }
export module FOO;
// { dg-module-cmi FOO }
namespace Outer {
class Y;
class Inner {
  class X;
  void Fn (X &, Y &); // #2
};
void Inner::Fn (X &, Y &) {}
}

// { dg-final { scan-assembler {_ZN5OuterW3FOO5Inner2FnERNS1_1XERNS_S0_1YE:} } }
