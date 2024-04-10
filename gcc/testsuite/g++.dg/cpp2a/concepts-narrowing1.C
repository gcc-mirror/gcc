// PR c++/104594
// { dg-do compile { target c++20 } }

template <unsigned char DIM_FROM>
concept Geometry = (DIM_FROM == -1);

template <class INIT>
requires Geometry<INIT::n>
auto GaussNewton(const INIT& init) -> void {}

template<int N>
struct X {
  static constexpr int n = N;
};

int main() { GaussNewton(X<-1>{}); } // { dg-error "no match|narrowing" }
