// PR c++/47544
// { dg-options -std=c++11 }
// { dg-final { scan-assembler "_ZN1sIiEC2Ev" } }
// { dg-final { scan-assembler-not "_ZN1sIiED2Ev" } }

template <typename T>
struct s {
  s();
  ~s() = default;
};

extern template struct s<int>;

template <typename T>
s<T>::s() = default;

template struct s<int>;

s<int> a;
