// Build don't link:
// Test resolution of templatized overloaded constructors.
// The more specialized constructor, i.e., A (const B<Dim1,Dim2> &b)
// should be chosen per \S 14.5.5.2/2 [temp.func.order/2].

template <int Dim1, int Dim2>
struct B {
  int f;
};

struct A {
  template <int Dim1, int Dim2>
  A (const B<Dim1,Dim2> &b) {}

  template <typename T>
  A (const T &b) {}
};

int
main ()
{
  B<2,3> b;
  A a (b);
  return 0;
}
