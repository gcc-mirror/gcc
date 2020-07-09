// PR c++/96132
// { dg-do compile }

template <int> class a;

template <int b> class c {
  a<b> e;
  void operator=(c d) { e = d; }
};
