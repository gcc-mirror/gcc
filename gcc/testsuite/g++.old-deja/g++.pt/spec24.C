// { dg-do assemble  }

template <class T> class A;
// template <>
class A<int>; // { dg-error "" "" { xfail *-*-* } } missing template header - 
