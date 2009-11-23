// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/14777
// { dg-do compile }

template <typename T>
struct B
{
protected:
  typedef int M; // { dg-error "protected" }
};

template <typename T>
struct A : B<T> {
  typedef typename B<char>::M N; // { dg-error "context" }
  A (int = N ());
};

A<int> a = A<int> ();
