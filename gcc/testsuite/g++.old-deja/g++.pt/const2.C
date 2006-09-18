// { dg-do link }
// This test should get a linker error for the reference to A<int>::i.
// { dg-error "i" "" { target *-*-* } 0 }

template <class T> struct B { static const int i = 3; };
template <class T> struct A { static const int i = B<T>::i; };
const int *p = &A<int>::i;

int main ()
{
  // Examine p to prevent optimising linkers from discarding it.
  return (p != 0);
}
