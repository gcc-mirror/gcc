// { dg-do assemble  }

template <class T, int i>
struct S1;

template <class T, int i, int j>
struct S2
{
  typedef typename S1<T, (i >= j ? 0 : 1) >::type type;
};
