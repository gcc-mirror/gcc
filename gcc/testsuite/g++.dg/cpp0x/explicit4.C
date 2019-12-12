// Negative explicit conv test.
// { dg-do compile { target c++11 } }

struct A {
  A(const A&, int = 0);		// { dg-message "note" }
};
struct B
{
  explicit operator A();
};

int main()
{
  B b;
  (A(b));			// OK
  (A(b,1));			// { dg-error "cannot convert" }
}
