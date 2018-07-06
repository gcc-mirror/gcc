// PR debug/80461
// { dg-do compile }
// { dg-options "-g -O" }

template <typename> class A;
struct B
{
  template <typename T, typename U>
  static bool foo (U T::*) { return true; }
};
template <typename, typename> class J;
template <typename T, typename U, typename V, typename... W>
class J<V (W...), U T::*> : public J<void(), U T::*> {};
template <typename T, typename U, typename... W>
class J<void(W...), U T::*> : public B {};
template <typename V, typename... W> struct A<V (W...)>
{
  template <typename, typename> using K = int;
  template <typename L, typename = K<int, void>, typename = K<int, void>> A (L);
};
template <typename V, typename... W>
template <typename L, typename, typename>
A<V (W...)>::A (L x) { J<V (), L>::foo (x); }
struct N;
volatile int v;

template <class O, class P>
void
bar ()
{
  O q;
  A<P> f = q;
  v++;
}

void
baz ()
{
  bar<int (N::*) (...) &, int()> ();
  bar<int (N::*) (...) const &, int()> ();
  bar<int (N::*) (...) volatile &, int()> ();
}
