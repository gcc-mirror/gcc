// { dg-do assemble  }
// Origin: Chris Heath <cheath@math.lsa.umich.edu>

struct A {
  template<typename T> explicit A(T t) {}
};

void f(A a) {}

int main() {f(1);} // { dg-error "" } no conversion from int to A.
