// PR c++/9634, c++/29469, c++/29607
// Contributed by: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR224: Make sure that a name is *truly* semantically dependent.

struct D {
  typedef int K;
};

template <typename T>
struct A
{
  typedef int Bar;

  template <typename>
  struct N {};

  typedef Bar          type1;
  typedef A::Bar       type2;
  typedef A<T>::Bar    type3;
  typedef A<T*>::Bar    type4;  // { dg-error "" "" { target c++17_down } }
  typedef typename A<T*>::Bar type5;

  typedef N<int>       type6;
  typedef A::N<int>    type7;
// { dg-error "" "" { target c++2a } .-1 }
  typedef A<T>::N<int> type8;
// { dg-error "" "" { target c++2a } .-1 }
  typedef A<T*>::template N<int> type9;  // { dg-error "" "" { target c++17_down } }
  typedef typename A<T*>::template N<int> type10;

  typedef D Bar2;
  struct N2 { typedef int K; };

  // Check that A::N2 is still considered dependent (because it
  //  could be specialized), while A::Bar2 (being just ::D) is not.
  typedef A::Bar2 type11;
  typedef type11::K k3;

  typedef A::N2 type12;
  typedef typename type12::K k2;
  typedef type12::K k1;  // { dg-error "" "" { target c++17_down } }

  // Check that A::Bar2 is not considered dependent even if we use
  // the typename keyword.
  typedef typename A::Bar2 type13;
  typedef type13::K k4;
};
