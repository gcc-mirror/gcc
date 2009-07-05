// Reduced from the testcase for c++/29433

template <class T>
struct A: T
{
  void f(typename T::type);
  using T::f;
  void g() { f(1); }
};

template <class T>
struct B: T
{ typedef int type; };

struct C
{
  typedef double type;
  void f();
};

int main()
{
  A<B<A<C> > > a;
  a.g();
}
