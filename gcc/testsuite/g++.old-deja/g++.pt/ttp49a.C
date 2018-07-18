// { dg-options -fnew-ttp-matching }

template <int i> class C {};
template <template <long> class TT> class D {};

int main()
{
  D<C> d;
}
