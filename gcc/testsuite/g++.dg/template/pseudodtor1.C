// PR c++/32384
// { dg-do compile }

struct A
{
  typedef int T;
  T foo ();

  A () { foo ().~T (); }
};

template<typename> struct B
{
  typedef int T;
  T foo ();

  B () { foo ().~T (); }
};

template<typename T> struct C
{
  T t;
  C () { t.~T (); }
};

template<typename S> struct D
{
  typedef int T;
  S foo ();

  D () { foo ().~T(); }
};

struct Z
{
  Z () {}
  ~Z () {}
};

A a;
B<int> b;
C<int> c1;
C<Z> c2;
D<int> d;
