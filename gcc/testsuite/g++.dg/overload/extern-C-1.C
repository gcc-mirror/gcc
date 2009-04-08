// PR c++/34691

namespace A {
  extern "C" void f(int = 5);	// { dg-message "= 5" }
}
namespace B {
  extern "C" void f(int = 4);	// { dg-message "= 4" }
}

using A::f;
using B::f;
int main() {
  void (*fp)(int) = f;		// OK
  f(3);				// OK
  f();				// { dg-error "default argument mismatch" }
}

