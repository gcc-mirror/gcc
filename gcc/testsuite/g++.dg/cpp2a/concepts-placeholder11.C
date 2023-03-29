// PR c++/107188
// { dg-do compile { target c++20 } }

namespace N {
  template<class, class> concept C = true;
}

struct X {
  N::C<int> auto f() { return 0; }
};
