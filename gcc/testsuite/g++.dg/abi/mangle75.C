// PR c++/91377
// { dg-do compile { target c++11 } }

struct f {
  static constexpr int d = 3;
  typedef int e;
};
template <int a> struct x { };
template <typename g, g j, g m> using n = x<j + m>;
template <typename ac> auto v() -> n<typename ac::e, 0, ac::d>;
void af() { v<f>(); }

// { dg-final { scan-assembler "_Z1vI1fE1xIXplLi0EsrT_1dEEv" } }
