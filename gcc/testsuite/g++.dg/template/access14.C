// PR c++/14777

template <typename T>
struct B
{
protected:
  typedef int M;
};

template <typename T>
struct A : B<T> {
  typedef typename B<T>::M N;
  A (int = N ());
};

A<int> a = A<int> ();
