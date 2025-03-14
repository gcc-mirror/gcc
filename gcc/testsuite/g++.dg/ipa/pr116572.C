/* { dg-do compile } */
/* { dg-options "-std=c++20 -O3 -fsanitize=undefined" } */

long v;
template <class> struct A;
template <typename C, typename = A<C>, typename = C>
class B;
template <>
struct A<char>
{
  static int foo(char *s, const char *t, long n) { return __builtin_memcmp(s, t, n); }
};
template <typename C, typename, typename>
struct B {
  long b;
  B(const C *);
  C *bar() const;
  constexpr unsigned long baz(const C *, unsigned long, unsigned long) const noexcept;
  void baz() { C c; baz(&c, 0, v); }
};
template <typename C, typename D, typename E>
constexpr unsigned long
B<C, D, E>::baz(const C *s, unsigned long, unsigned long n) const noexcept
{
  C *x = bar(); if (!x) return b; D::foo(x, s, n); return 0;
}
namespace {
struct F { virtual ~F() {} };
struct F2 { virtual void foo(B<char>) const; };
struct F3 : F, F2 { void foo(B<char> s) const { s.baz(); } } f;
}
int
main()
{
  F *p;
  dynamic_cast<F2 *>(p)->foo("");
}
