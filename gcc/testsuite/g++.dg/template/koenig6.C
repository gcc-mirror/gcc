// PR c++/38850

template <typename VType>
class Vector2 {
 private:
  VType c_[2];
 public:
  typedef Vector2<VType> Self;

  Vector2(const VType x, const VType y) {
    c_[0] = x;
    c_[1] = y;
  }

  friend inline Self Max(const Self &v1, const Self &v2) {
    return Self(v1.c_[0], v1.c_[1]);
  }
};

template <class T>
Vector2<float> foo(T x) {
  Vector2<float> y(0,0);
  return Max(y, y);
}

int main() {
  foo(3);
  return 0;
}
