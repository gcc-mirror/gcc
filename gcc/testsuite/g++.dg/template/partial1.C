// { dg-do run }
// Origin: Jo Totland <jototland@hotmail.com>

// PR c++/6620
// Partial specialization involving expression of non-type template
// parameter causes ICE.

extern "C" void abort();

template <int N> struct HoldInt
{
};

template <class A, class B> struct Add
{
};

template <int N> struct Add<HoldInt<N>, HoldInt<-N> >
{
  typedef int type;
  int f() { return 0; }
};

template <int N, int M> 
struct Add<HoldInt<N>, HoldInt<M> >
{
  typedef HoldInt<N+M> type;
  int f() { return 1; }
};

int main() {
  Add<HoldInt<1>, HoldInt<-1> > a;
  Add<HoldInt<1>, HoldInt<-2> > b;
  if (a.f() != 0 || b.f() != 1)
    abort();
}
