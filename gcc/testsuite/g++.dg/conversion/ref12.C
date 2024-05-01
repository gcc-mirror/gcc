// PR c++/113141

struct Matrix { };

struct TPoint3 { operator const Matrix(); };

void f(Matrix&);

int main() {
  TPoint3 X;
  Matrix& m = (Matrix &)X;
  f((Matrix &)X);
}
