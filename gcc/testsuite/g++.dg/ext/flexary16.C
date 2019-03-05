// PR c++/71147 - [6 Regression] Flexible array member wrongly rejected
//   in template
// { dg-do compile }
// { dg-options "-Wpedantic -Wno-error=pedantic" }

template <typename>
struct container
{
  struct elem {
    unsigned u;
  };

  struct incomplete {
    int x;
    elem array[];  // { dg-warning "10:ISO C\\+\\+ forbids flexible array member" }
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
    typename container<T>::elem array[];  // { dg-warning "33:ISO C\\+\\+ forbids flexible array member" }
  };
};


unsigned g (D<void>::S *s)
{
  return s->array [0].u;
}
