// { dg-do assemble  }

template <class T>
struct S1
{
};

template <class T>
struct S2
{
  typedef T* pointer_t;
};

int f(S2<S1<int> >::pointer_t p1, S2<S1<int> >::pointer_t p2)
{
  return (int) (p1 - p2);
}
