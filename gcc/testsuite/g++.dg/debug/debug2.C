/* Verify that sched re-inserts needed scopes properly.  */
/* { dg-do compile } */
/* { dg-options "-mcpu=ev5" { target alpha*-*-* } } */

template <class T>
inline void foo()
{
  void (T::*x)() __attribute__ ((__unused__)) = &T::bar;
}

template <class T>
struct D
{
  void bar() {
  }
  T i;
};

template <class T>
struct E
{
  void bar() {
    foo <D<T> > ();
    *i-- = *i;
  }
  T i;
};

struct A {};
template<typename T> struct B { typedef typename T::t t; };
template<typename T> struct B<T*> { typedef T& t; };
template<typename T>
struct C
{
  T b;
  explicit C (const T& i) : b (i) { }
  typename B<T>::t operator* () const { return *b; }
  C operator-- (int) { return C (b--); }
};

template void foo <E<C<A**> > > ();
template void foo <D<C<A**> > > ();
