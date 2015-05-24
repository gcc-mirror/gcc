#include <memory>
namespace {
class A {
  bool a;
};
}
class H {
  std::unique_ptr<A> bar() const;
};
std::unique_ptr<A> H::bar() const { return std::make_unique<A>(); }

