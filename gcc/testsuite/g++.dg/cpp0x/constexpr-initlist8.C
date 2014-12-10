// PR c++/63415
// { dg-do compile { target c++11 } }

template <typename T>
struct A {
  static constexpr int value = int(T{});
};
