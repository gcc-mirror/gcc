// { dg-do assemble  }

template <int I>
struct S1 { };

template <class T>
struct S2 {
  enum { x = 3 };

  void f(S1<x>&);
};

template <class T>
void S2<T>::f(S1<x>&)
{
}
