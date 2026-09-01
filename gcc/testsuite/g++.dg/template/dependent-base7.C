// PR c++/114804
// { dg-do compile { target c++11 } }

template <int D> struct blurb
{
  constexpr static int d = D;
};

template <int> struct pant
{
};

template <typename Base> struct bug : Base
{
  using Base::d;
  struct problem : pant<d>
  {
  };
};

template struct bug<blurb<2>>;
