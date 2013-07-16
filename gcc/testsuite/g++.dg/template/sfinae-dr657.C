// DR 657
// Test that a return or parameter type with abstract class type causes a
// deduction failure.

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
  int i = declval<A>();
  i = arg<A>(1);
}
