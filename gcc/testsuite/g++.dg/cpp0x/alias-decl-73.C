// PR c++/100102
// { dg-do compile { target c++11 } }

template <bool B1> using a = int;
template <class T3, class T4> struct k {
  static long o();
  template <class T5> using n = a<bool(k::o)>;
  n<int> q;
};
