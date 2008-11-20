// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/37142
// { dg-do compile }

template<typename T, const T a, template <typename U, U u> class W> struct A {};

template<typename T, const T t> struct B {};

int
main ()
{
  A<long, 0, B> a;
  return 0;
}


