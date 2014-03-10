// PR c++/47049
// { dg-do compile { target c++11 } }

enum { E = 0, F = 1 };
template <int N, int M = ((N == 1) ? F : E)> class S {};
template <int N>
struct T
{
  static void
  foo (S<N> *p)
  {
    S<N> u;
    [&u] ()->bool {} ();
  }
};

int main()
{
  T<0>().foo(0);
}
