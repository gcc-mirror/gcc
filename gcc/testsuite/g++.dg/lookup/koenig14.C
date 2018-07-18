// ADL can be recursive (via instantiation), make sure that works.

namespace X
{
  class B {};
  
  void frob ();
  int frob (B); // Inner ADL resolves here
}

void frob (int);
void frob (float);

namespace Y
{
  struct A {};
  void frob (void*, void *, void *); // Outer ADL resolves here
}

template <typename T, typename U>
struct C : U
{
  int ary[sizeof frob(T())]; // ADL occurs here during instantiation
};

void Foo (C<X::B, Y::A> *p, X::B *q)
{
  frob(q, p, q); // ADL causes instantation of C<...>
  // We will have already searched X by the time the instantation happens
}
