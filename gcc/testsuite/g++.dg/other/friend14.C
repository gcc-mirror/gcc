// PR c++/59480

class Matrix;

Matrix rot90 (const Matrix& a, int k = 1);
template<typename> Matrix rot90_ (const Matrix& a, int k = 1);

class Matrix {
  friend Matrix rot90 (const Matrix&, int);
  template<typename> friend Matrix rot90_ (const Matrix&, int);
};

Matrix rot90 (const Matrix& a, int k) { return Matrix(); }
template<typename> Matrix rot90_ (const Matrix& a, int k) { return Matrix(); }
