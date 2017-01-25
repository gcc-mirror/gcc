// PR c++/69251 - [6 Regression] ICE (segmentation fault) in unify_array_domain
// on i686-linux-gnu
// { dg-do compile }
// { dg-additional-options "-Wno-error=pedantic" }

struct A {
  int n;
  char a[];   // { dg-warning "forbids flexible array member" }
};

template <class>
struct B;

// The following definition shouldn't be needed but is provided to prevent
// the test from failing with an error due to PR c++/69349 - template
// substitution error for flexible array members.  (This doesn't compromise
// the validity of this test since all it tests for is the absennce of
// the ICE.)
template <class>
struct B { typedef int X; };

template <class T>
struct B<T[]> { typedef int X; };

template <class T>
struct C { typedef typename B<T>::X X; };

template <class T>
int foo (T&, typename C<T>::X = 0)
{
  return 0;
}

void bar (A *a)
{
  foo (a->a);
}
