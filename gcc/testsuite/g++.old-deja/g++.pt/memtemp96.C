// Build don't link:
// Origin: Jason Merrill <jason@cygnus.com>

template <class T> struct A {
  template <class U> void f(U);
};

template <>
template <class U>
void A<int>::f(U);

A<int> a;

void g ()
{
  a.f (3);
}
