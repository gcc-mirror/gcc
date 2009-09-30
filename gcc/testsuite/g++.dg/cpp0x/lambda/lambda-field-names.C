// "For each entity captured by copy, an unnamed non-static data member is
// declared in the closure type" -- test that there isn't a member of the
// closure with the same name as the captured variable.

// { dg-options -std=c++0x }

template <class T>
struct A: public T
{
  A(T t): T(t) { }
  int f() { return this->i; }	// { dg-error "" "no member named i" }
};

int main()
{
  int i = 42;
  auto lam = [i]{ };
  lam.i = 24;			// { dg-error "" "no member named i" }
  A<decltype(lam)> a(lam);
  return a.f();
}
