// PR c++/84691
// { dg-do compile { target c++11 } }

template<typename>
struct a {
  unsigned b = [] {
    union {
      friend void c() {}  // { dg-error "local class" }
    };  // { dg-error "no members" }
  };
};
