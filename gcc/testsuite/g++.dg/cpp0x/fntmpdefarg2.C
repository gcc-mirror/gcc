// PR c++/46831
// { dg-options -std=c++0x }

struct B { };
struct D : B { };
struct A {
  template<typename T = void> operator D&(); // { dg-message "template conversion" }
  operator long();
};

void f(long);
void f(B&);

struct A2 {
  template<typename T = void> operator B&();
};

void f2(const B&);

int main() {
  f(A());
  f2(A2());
  f2(A());			// { dg-error "" }
}
