// PR c++/102307
// { dg-do compile { target c++11 } }

#include <array>
template <unsigned N, unsigned M> struct Matrix {
  constexpr Matrix(double const (&arr)[N][M]); // { dg-warning "never defined" }
  constexpr Matrix(std::array<std::array<double, M>, N> const &arr);
};
int main() {
  constexpr Matrix<2, 3>
    mat {{ {1.0, 2.0, 3.0}, {4.0, 5.0, 6.0} }}; // { dg-error "before its definition" }
}
