// PR c++/32241
// { dg-do compile }

struct A
{
  typedef int T;
  T &foo ();
  A () { foo.~T (); }	// { dg-error "10:invalid use of member function|expected" }
};

template <typename T> struct B
{
  T &foo ();
  B () { foo.~T (); }	// { dg-error "15:invalid use of member" }
};

B<int> b;

template <typename T, typename S> struct C
{
  T t;
  C () { t.~S (); }	// { dg-error "13:is not of type" }
};

C<int, long int> c;

template <typename T> struct D
{
  T t;
  typedef long int U;
  D () { t.~U (); }	// { dg-error "10:is not of type" }
};

D<int> d;

template <typename T> struct E
{
  T &foo ();
  typedef long int U;
  E () { foo.~U (); }	// { dg-error "10:is not of type" }
};

E<int> e;
