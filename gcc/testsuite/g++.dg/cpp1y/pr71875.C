// PR c++/71875
// { dg-do link { target c++14 } }

template <typename T>
constexpr bool IsMatrix = false;

template<typename TElem>
class Matrix {};

template <typename TElem>
constexpr bool IsMatrix<Matrix<TElem>> = true;

template<typename TNestVec>
class RowVecExpMatrix;

template <typename TNestVec>
constexpr bool IsMatrix<RowVecExpMatrix<TNestVec>> = true;

int
main ()
{
  static_assert (IsMatrix<RowVecExpMatrix<Matrix<int>>>, "Matrix check error");
  static_assert (IsMatrix<Matrix<int>>, "Input type is not a matrix");
}
