// Testcase for not trying a candidate that would generate an ill-formed
// instantiation.

template <int N> struct A {
  int ar[N];
};

template <int N> struct B {
  B () { };
  B (const A<N> &) { };
  B (const A<N-1> &, int);
};

int
main ()
{
  A<1> a;
  B<1> b1;
  B<1> b2 (a);
}

