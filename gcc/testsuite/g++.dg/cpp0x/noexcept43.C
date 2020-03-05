// PR c++/90490
// { dg-do compile { target c++11 } }

struct R { constexpr operator bool() { return false;} };

template <typename>
struct S {
  void g() noexcept(decltype(R{ }) { }) {
  }
};
