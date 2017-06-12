// PR c++/77563

struct A {
  A(int) {}
  A(unsigned) {}  // Comment to make it work

  explicit A(long) {}  // Comment to make it work
};

void f(A) { }

int main() {
  f(2);
  f(3l);			// { dg-error "ambiguous" }
}
