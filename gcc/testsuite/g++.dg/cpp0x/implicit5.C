// Test that the default B copy constructor calls the A member template
// constructor.
// { dg-options -std=c++0x }

struct A
{
  A() = default;
  A(A&&) = default;
  template <class T>
  A(const T& t) { t.i; }	// { dg-error "no member" }
};

struct B: A { };

int main()
{
  B b;
  B b2(b);
}
