// PR c++/99901
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler-not "_ZN1A1aE" } }
// { dg-final { scan-assembler-not "_ZN2A21aE" } }
// { dg-final { scan-assembler-not "_ZN1CIiE1cE" } }
// { dg-final { scan-assembler "_ZN1B1bE" } }
// { dg-final { scan-assembler "_ZN2B21bE" } }
// { dg-final { scan-assembler "_ZN2B31bE" } }

struct A {
  static const int a = 5;
};

struct A2 {
  static constexpr int a = 5;
};

struct B {
  static const int b;
};
constexpr int B::b = 5;

struct B2 {
  static const int b = 5;
};
constexpr int B2::b;

struct B3 {
  static constexpr int b = 5;
};
const int B3::b;

template <class T>
struct C {
  static constexpr int c = 5;
};
template <class T>
constexpr int C<T>::c;

int i = C<int>::c;
