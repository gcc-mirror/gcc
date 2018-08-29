// PR c++/46831
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct B { };
struct D : B { };
struct A {
  template<typename T = void> operator D&();
  operator long();
};

template <> A::operator D&(); // { dg-message "template conversion" }

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
