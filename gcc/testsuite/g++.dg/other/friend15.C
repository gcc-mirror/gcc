// PR c++/59480

class Matrix;

void rot90 (const Matrix& a, int k = 1) { }
template<typename> void rot90_ (const Matrix& a, int k = 1) { }

class Matrix {
  friend void rot90 (const Matrix&, int);
  template<typename> friend void rot90_ (const Matrix&, int);
};

void rot90 (const Matrix& a, int k);
template<typename> void rot90_ (const Matrix& a, int k);
