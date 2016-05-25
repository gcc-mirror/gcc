// PR c++/71147 - [6 Regression] Flexible array member wrongly rejected
//   in template
// { dg-do compile }

template <typename>
struct container
{
  struct elem {
    unsigned u;
  };

  struct incomplete {
    int x;
    elem array[];
  };
};

unsigned f (container<void>::incomplete* i)
{
  return i->array [0].u;
}


template <typename T>
struct D: container<T>
{
  struct S {
    int x;
    typename container<T>::elem array[];
  };
};


unsigned g (D<void>::S *s)
{
  return s->array [0].u;
}
