// { dg-do run  }
// Test for Koenig lookup involving overloaded functions.

namespace N1 {
  struct A { };
  void f1(A) {}
  void f2(float) {}
  void g(void (*)(float)) {}
}

using N1::f1;
void f1(float) {}

using N1::f2;
template <class T>
void f2(N1::A, T) {}

void g(void (*)(int)) {}

int main() {  
   g(&f1); // Works?
   g(f2); // Works?
}
