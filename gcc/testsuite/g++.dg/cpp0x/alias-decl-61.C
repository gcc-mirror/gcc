// PR c++/83714
// { dg-do compile { target c++11 } }

class a {
  typedef int b;
  operator b();
};
struct c {
  using d = a;
};
using e = c;

template <class T>
e f(T) {
  return e::d {};		// { dg-error "could not convert" }
}
