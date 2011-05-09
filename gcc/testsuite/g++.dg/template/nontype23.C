// PR c++/48936

template <bool C> int foo (void);
template <class T> struct S
{
  static const unsigned int a = sizeof (T);
  enum { c = sizeof (foo <(a == 0)> ()) };
};
S<int> x;
