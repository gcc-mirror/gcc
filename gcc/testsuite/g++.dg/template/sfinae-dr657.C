// DR 657 SUPERSEDED BY P0929
// Test that a return or parameter type with abstract class type DOES NOT cause
// a deduction failure or conversion failure.

struct A
{
  A();
  A(int);
  virtual void f() = 0;
};

template<class T> T declval();
template<class T> int declval(...);

template<class T> void arg(T);
template<class T> int arg(...);

int main()
{
  int i = declval<A>();		// { dg-error "ambiguous" }
  i = arg<A>(1);		// { dg-error "abstract" }
}
