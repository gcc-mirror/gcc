// PR c++/84720
// { dg-do compile { target c++11 } }

template<int &&>
struct a {
  template<typename...>
  static void b() {
    b();
  }
};
