// PR c++/18140
// { dg-options "-std=gnu++98" }

template <int N> struct IntHolder {
  static const int value = N;
};

template <int N, int S> struct ShrIntHolder {
  static const int value = IntHolder< N>>S >::value;
};

