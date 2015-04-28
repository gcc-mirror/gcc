// PR c++/65734
// { dg-do compile { target c++11 } }

template <typename T>
struct BVector
{
  T t;
};
BVector<int> m;

template <template <class> class T>
struct BV2
{
  typedef T<float> value_type alignas (16);
  value_type v;
};
BV2<BVector> m2;

#define SA(X) static_assert((X),#X)
SA(alignof (BV2<BVector>::value_type) == 16);
