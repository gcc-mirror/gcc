// Build don't link:
// Origin: Chris Heath <cheath@math.lsa.umich.edu>

struct A {
  template<typename T> explicit A(T t) {}
};

void f(A a) {}

int main() {f(1);} // ERROR - no conversion from int to A.
