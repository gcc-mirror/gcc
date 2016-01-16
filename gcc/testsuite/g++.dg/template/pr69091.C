// PR c++/69091
// { dg-do compile { target c++14 } }

template <class ValueType, ValueType>
struct Option {};

template <class ValueType, ValueType Value, class OptionsRhs>
auto operator|(Option<ValueType, Value>, OptionsRhs) {
  return Value;
}

enum canine_t { no, yes };
Option<canine_t, no> cat;
Option<canine_t, yes> dog;

template <class T>
void f(T) {
  cat | dog;
}

struct A {};
int main() {
  f(A{});
  return 0;
}
