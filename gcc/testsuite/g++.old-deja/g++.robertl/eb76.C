//Build don't link:
// from include/g++/stl_relops.h
template <class T>
inline bool operator!=(const T& x, const T& y) {
  return !(x == y);
}

enum T {
  V1,
};                           // ERROR -

struct X {
  T      t : 31;
};

void
f(X& v) {
  if( v.t != V1 ) {
  }
}
