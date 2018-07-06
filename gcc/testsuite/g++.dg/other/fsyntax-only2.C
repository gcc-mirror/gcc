// PR c++/79490
// { dg-options -fsyntax-only }

struct IRegistryHub {
  virtual ~IRegistryHub();
};
namespace {
class A : IRegistryHub {};
A *fn1();
}
void fn2() { delete fn1(); }
