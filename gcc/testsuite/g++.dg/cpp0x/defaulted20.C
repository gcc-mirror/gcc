// PR c++/46497
// { dg-do compile { target c++11 } }

struct A {
  A(A&&) = default;
};
struct B {
  const A a;
  B(const B&) = default;
  B(B&&) = default;	// { dg-error "implicitly deleted|use of deleted" }
};

void g(B);			// { dg-message "argument 1" }
B&& f();

int main()
{
  g(f());			// { dg-error "deleted" }
}
