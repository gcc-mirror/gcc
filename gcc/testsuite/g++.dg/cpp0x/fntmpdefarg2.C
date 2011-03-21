// PR c++/46831
// { dg-options -std=c++0x }

struct B { };
struct D : B { };
struct A {
  template<typename T = void> operator D&();
  operator long();
};

void f(long);
void f(B&);

int main() { f(A()); }
