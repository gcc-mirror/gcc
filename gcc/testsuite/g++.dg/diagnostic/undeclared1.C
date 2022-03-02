// PR c++/79493

namespace A { }
struct B {
  void f(A::nonexistent param); // { dg-error ".A::nonexistent. has not been declared" }
  void* g(A::nonexistent param); // { dg-error ".A::nonexistent. has not been declared" }
};
