// PR c++/107461
// { dg-do compile { target c++11 } }

template<class T>
constexpr T min(T t0, T t1) {
  return t0 < t1 ? t0 : t1;
}

template<int MAX>
struct Matrix;

template<int MAXOP, int other_MAXOP>
Matrix<min(MAXOP, other_MAXOP)>
operator+(Matrix<MAXOP> const& lhs, Matrix<other_MAXOP> const& rhs); // #1

template<int MAX>
struct Matrix {
  template<int MAXOP, int other_MAXOP>
  friend Matrix<min(MAXOP, other_MAXOP)>
  operator+(Matrix<MAXOP> const& lhs, Matrix<other_MAXOP> const& rhs); // #2
};

int main() {
  Matrix<1> a;
  a+a;
}
