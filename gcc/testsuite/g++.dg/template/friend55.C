// PR c++/59956

template <int I> struct A;
template <int I> class B {
  int i;
  template <int A_S> friend void A<A_S>::impl();
};

B<0> b1;
template<int I>struct A { void impl(); };
B<1> b2;

template<int I> void A<I>::impl() { ++b1.i; ++b2.i; }

int main()
{
  A<0>().impl();
}
