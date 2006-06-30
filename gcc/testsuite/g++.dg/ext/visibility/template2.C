// PR c++/27000
// Implicitly instantiated templates should not be affected by
// #pragma visibility.

/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-not-hidden "_ZN1SIiED1Ev" } } */
/* { dg-final { scan-not-hidden "_ZN1SIiEC1ERKi" } } */

template <class T>
struct S
{
  S (const T &);
  ~S ();
  T t;
};

template <class T>
S<T>::S (const T &x)
{
  t = x;
}

template <class T>
S<T>::~S ()
{
}

#pragma GCC visibility push(hidden)
struct U
{
  S<int> s;
  U () : s (6) { }
} u;
#pragma GCC visibility pop
