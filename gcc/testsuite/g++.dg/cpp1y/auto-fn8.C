// { dg-options "-std=c++1y -pedantic-errors" }

auto f() { return 42; }		// { dg-error "old declaration .auto" }
auto f();			// OK
int f();			// { dg-error "new declaration" }

template <class T> auto f(T t) { return t; }
template <class T> T f(T t);

int main()
{
  f(42);			// { dg-error "ambiguous" }
}
