// { dg-do compile }
// { dg-options "-g" }
// Origin: <jan at etpmod dot phys dot tue dot nl>
// PR c++/19508: avoid attributes for template parameters

template <typename T>
struct BVector
{
  typedef T T2;
  typedef T value_type __attribute__ ((aligned(8)));    // { dg-bogus "attribute" "attribute" }
  typedef T2 value_type2 __attribute__ ((aligned(8)));  // { dg-bogus "attribute" "attribute" }
  value_type v;
};
BVector<int> m;

template <template <class> class T>
struct BV2
{
  typedef T<float> value_type __attribute__((aligned(8))); // { dg-bogus "attribute" "attribute" }
  value_type v;
};
BV2<BVector> m2;

