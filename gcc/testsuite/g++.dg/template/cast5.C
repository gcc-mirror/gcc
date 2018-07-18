// PR c++/82360
// { dg-do compile { target c++11 } }

class a {};
template <class> class b {
  b(b &&c) : d(static_cast<a &&>(c.d)) {}
  a d;
};
