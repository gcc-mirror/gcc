// PR c++/93597 - ICE with lambda in operator function.
// { dg-do compile { target c++11 } }

template <typename T>
struct S {
  using T ::operator<;
  void operator==(T x) { [x] { 0 < x; }; }
};
