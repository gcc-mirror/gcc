
// Contributed by Douglas Gregor <doug.gregor@gmail.com>

template<class T> struct wrap {};

template<typename T> bool& operator==(wrap<T>, wrap<T>);

template<typename T>
void g(T, wrap<wrap<int> > x)
{
  bool& b = x == x; // { dg-bogus "invalid initialization of reference" "" { xfail *-*-*} }
}

template<typename T> int& operator==(wrap<wrap<T> >, wrap<wrap<T> >);

void h()
{
  wrap<wrap<int> > x;
  g(17, x);
}
