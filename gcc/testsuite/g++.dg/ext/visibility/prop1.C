// Test for propagation of visibility through template arguments

// { dg-do compile }
// { dg-require-visibility "" }
// { dg-final { scan-hidden "_Z1fIN1N1AEEvT_" } }
// { dg-final { scan-hidden "_Z1hIXadL_ZN1N1iEEEEvv" } }

namespace N __attribute ((__visibility__ ("hidden")))
{
  struct A { };
  int i;
}

template <class T> void f (T) { }
template <int *I> void h() { }

void g()
{
  N::A a;
  f(a);
  h<&N::i>();
}

