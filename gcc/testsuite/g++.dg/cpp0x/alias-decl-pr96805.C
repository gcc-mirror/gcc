// PR c++/96805
// { dg-do compile { target c++11 } }

template <typename T> class a {
  template <int N> struct c {
    template <bool B> using t = int;
    t<N> m;
  };
};
