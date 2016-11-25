/* { dg-do compile } */
/* { dg-options "-O2 -std=c++11" } */

struct A {};
struct B {};
struct C { using p = int *; template <typename> using ra = A; };
struct J : C { template <typename> struct K { typedef C::ra<int> o; }; };
template <typename> struct D
{
  struct H : J::K<int>::o { H (J::p, A) : J::K<int>::o () {} };
  H d;
  D (const char *, const A &x = A ()) : d (0, x) {}
};
extern template class D<char>;
enum L { M };
struct F { virtual char *foo (); };
template <class> struct I : B { static int foo (int) {} };
struct G { typedef I<int> t; };
void foo (int) { G::t::foo (0); }
void bar (const D<char> &, const D<int> &, int, L);
void baz () try { foo (0); } catch (F &e) { bar (e.foo (), "", 0, M); }
