// { dg-do run { target i?86-*-* } }
// { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } }
// Test for proper handling of attributes in template instantiation.
// Contributed by Jason Merrill <jason@cygnus.com>

template <class T>
struct A {
  static void f () __attribute__ ((stdcall));
};

template <class T> void
A<T>::f () { }

void g (void (__attribute__ ((stdcall)) *p)()) { }
void g (int);

int
main ()
{
  g (&A<int>::f);
}
