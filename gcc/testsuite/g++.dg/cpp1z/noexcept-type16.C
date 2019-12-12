// PR c++/80614
// { dg-do compile { target c++17 } }

template <typename T> void fn() {}

int main() {
  // { dg-final { scan-assembler "_Z2fnIKFvvEEvv" } }
  fn<void() const>();
  // { dg-final { scan-assembler "_Z2fnIKDoFvvEEvv" } }
  fn<void() const noexcept>();
}
