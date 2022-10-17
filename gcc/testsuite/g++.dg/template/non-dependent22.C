// PR c++/104565
// { dg-do compile { target c++11 } }

struct apa {
  constexpr int n() const { return 3; }
};

template<class>
int f() {
  apa foo;
  return int{foo.n()};  // no matching function for call to 'apa::n(apa*)'
}
