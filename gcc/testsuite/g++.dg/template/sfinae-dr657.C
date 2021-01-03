// DR 657 SUPERSEDED BY DR 1646
// Test that a return or parameter type with abstract class type DOES NOT cause
// a deduction failure, but there is no implicit conversion sequence for
// a parameter of abstract class type.

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
  i = arg<A>(1);
}
