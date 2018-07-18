// PR c++/81917
// { dg-do compile { target c++11 } }

template <typename> using a = void;
template <typename, typename = void> struct b
{
  typedef int c;
};
template <typename d> class b<d, a<typename d::e>>;
template <typename d, typename = typename b<d>::c> class f;
template <typename> class g { };
template <typename, typename> class h
{
  class i;
  typedef g<f<i>> j;
  class i
  {
    j k;			// { dg-error "incomplete" }
  };
};
h<int, int> H;

