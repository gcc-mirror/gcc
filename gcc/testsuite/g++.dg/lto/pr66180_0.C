// { dg-lto-do link }
// { dg-require-effective-target lto_incremental }
// { dg-lto-options { { -flto -std=c++14 -r -nostdlib } } }
#include <memory>
namespace {
class A {
  int i;
};
}
class G {
  std::unique_ptr<A> foo() const;
};
std::unique_ptr<A> G::foo() const { return std::make_unique<A>(); }

