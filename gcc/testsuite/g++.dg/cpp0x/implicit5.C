// Test that the default B copy constructor calls the deleted A
// copy constructor.
// { dg-options -std=c++0x }

struct A			// { dg-message "declares a move" }
{
  A() = default;
  A(A&&) = default;
  template <class T>
  A(const T& t) { t.i; }
};

struct B: A { };		// { dg-error "implicitly|use of deleted" }

int main()
{
  B b;
  B b2(b);			// { dg-error "deleted" }
}
