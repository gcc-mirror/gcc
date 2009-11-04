// PR c++/17365
// ADL should not find B::N.

namespace A
{
  namespace B
  {
    template <typename T> struct N {int n_;};
  }
  template <typename T> int N( T p ) { return p->n_; }
  template <typename T> void f( T p ) { N(p); }  // #1
}
int main()
{
  A::B::N<int> n;
  A::f(&n);
  return 0;
}
