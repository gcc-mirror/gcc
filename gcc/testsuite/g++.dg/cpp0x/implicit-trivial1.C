// PR c++/46807
// { dg-options -std=c++0x }
// In C++98/03, B::B(const B&) is trivial because A::A(const A&) is trivial,
// even though doing overload resolution would mean calling the template
// constructor.  In C++0x, we do overload resolution to determine triviality.

struct A
{
  A() {}
private:
  template <class T> A(T&);	// { dg-error "private" }
};

struct B			// { dg-error "implicitly deleted|this context" }
{
  mutable A a;
};

int main()
{
  B b;
  B b2(b);			// { dg-error "deleted" }
}
