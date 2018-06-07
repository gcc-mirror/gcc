// PR c++/84720
// { dg-do compile { target c++11 } }

template<int &&>		// { dg-error "not a valid type" }
struct a {
  template<typename...>
  static void b() {
    b();
  }
};
