// PR c++/93279
// { dg-do compile { target c++11 } }

template <typename T> struct B { using f = int; };
template <typename T, int N> struct E {
  template <typename U, typename B<E>::f = 0>
  void operator*(U l) { [l](T m) { m * l; }; }
};

int
main ()
{
  E<E<float, 4>, 1> n;
  n * 4.f;
}
