// PR c++/46497
// { dg-options -std=c++0x }

struct A {
  A(A&&) = default;		// { dg-message "A::A|no known conversion" }
};
struct B {
  const A a;
  B(const B&) = default;
  B(B&&) = default;		// { dg-error "implicitly deleted|no match" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 10 }
};

void g(B);			// { dg-error "argument 1" }
B&& f();

int main()
{
  g(f());			// { dg-error "deleted" }
}
