// PR c++/118632
// { dg-do compile { target c++11 } }

template<class T, T* = nullptr>
class Matrix {};

template<class T>
void operator*(Matrix<T>, int);

int main() {
  Matrix<int> m;
  m * 42;
}
